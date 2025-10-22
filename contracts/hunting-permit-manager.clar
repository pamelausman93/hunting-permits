;; hunting-permit-manager
;; Manages hunting permits with tag allocation and conservation fee collection

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-quota-exceeded (err u103))
(define-constant err-invalid-season (err u104))
(define-constant err-already-has-permit (err u105))

;; Data Variables
(define-data-var next-permit-id uint u1)
(define-data-var next-tag-id uint u1)

;; Data Maps
(define-map permits
  { permit-id: uint }
  {
    hunter: principal,
    hunter-name: (string-utf8 100),
    permit-type: (string-ascii 50),
    issued-at: uint,
    expires-at: uint,
    valid: bool,
    fees-paid: uint
  }
)

(define-map tags
  { tag-id: uint }
  {
    permit-id: uint,
    species: (string-ascii 50),
    zone: (string-ascii 20),
    season: (string-ascii 50),
    issued-at: uint,
    used: bool,
    used-at: (optional uint)
  }
)

(define-map species-quotas
  { species: (string-ascii 50), season: (string-ascii 50) }
  {
    total-quota: uint,
    allocated: uint,
    remaining: uint
  }
)

(define-map hunter-permits
  { hunter: principal }
  { permit-id: uint, active: bool }
)

(define-map permit-tags
  { permit-id: uint, tag-id: uint }
  { exists: bool }
)

;; Read-only functions
(define-read-only (get-permit (permit-id uint))
  (map-get? permits { permit-id: permit-id })
)

(define-read-only (get-tag (tag-id uint))
  (map-get? tags { tag-id: tag-id })
)

(define-read-only (get-species-quota (species (string-ascii 50)) (season (string-ascii 50)))
  (map-get? species-quotas { species: species, season: season })
)

(define-read-only (get-hunter-permit (hunter principal))
  (map-get? hunter-permits { hunter: hunter })
)

(define-read-only (has-valid-permit (hunter principal))
  (match (map-get? hunter-permits { hunter: hunter })
    entry
      (match (map-get? permits { permit-id: (get permit-id entry) })
        permit
          (and (get valid permit) (> (get expires-at permit) stacks-block-height))
        false)
    false)
)

;; Public functions

;; Set species quota (admin only)
(define-public (set-species-quota 
    (species (string-ascii 50))
    (season (string-ascii 50))
    (total-quota uint)
  )
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set species-quotas
      { species: species, season: season }
      {
        total-quota: total-quota,
        allocated: u0,
        remaining: total-quota
      })
    (ok true))
)

;; Issue hunting permit
(define-public (issue-permit
    (hunter principal)
    (hunter-name (string-utf8 100))
    (permit-type (string-ascii 50))
    (duration-days uint)
    (fees-paid uint)
  )
  (let
    ((permit-id (var-get next-permit-id))
     (expires-at (+ stacks-block-height (* duration-days u144))))
    
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-none (get-hunter-permit hunter)) err-already-has-permit)
    
    (map-set permits
      { permit-id: permit-id }
      {
        hunter: hunter,
        hunter-name: hunter-name,
        permit-type: permit-type,
        issued-at: stacks-block-height,
        expires-at: expires-at,
        valid: true,
        fees-paid: fees-paid
      })
    
    (map-set hunter-permits
      { hunter: hunter }
      { permit-id: permit-id, active: true })
    
    (var-set next-permit-id (+ permit-id u1))
    (ok permit-id))
)

;; Allocate tag to permit
(define-public (allocate-tag
    (permit-id uint)
    (species (string-ascii 50))
    (zone (string-ascii 20))
    (season (string-ascii 50))
  )
  (let
    (
      (permit (unwrap! (map-get? permits { permit-id: permit-id }) err-not-found))
      (quota (unwrap! (map-get? species-quotas { species: species, season: season }) err-not-found))
      (tag-id (var-get next-tag-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> (get remaining quota) u0) err-quota-exceeded)
    (asserts! (get valid permit) err-unauthorized)
    
    (map-set tags
      { tag-id: tag-id }
      {
        permit-id: permit-id,
        species: species,
        zone: zone,
        season: season,
        issued-at: stacks-block-height,
        used: false,
        used-at: none
      })
    
    (map-set permit-tags
      { permit-id: permit-id, tag-id: tag-id }
      { exists: true })
    
    (map-set species-quotas
      { species: species, season: season }
      (merge quota {
        allocated: (+ (get allocated quota) u1),
        remaining: (- (get remaining quota) u1)
      }))
    
    (var-set next-tag-id (+ tag-id u1))
    (ok tag-id))
)

;; Use tag (mark as used)
(define-public (use-tag (tag-id uint))
  (let
    (
      (tag (unwrap! (map-get? tags { tag-id: tag-id }) err-not-found))
      (permit (unwrap! (map-get? permits { permit-id: (get permit-id tag) }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get hunter permit)) err-unauthorized)
    (asserts! (not (get used tag)) err-unauthorized)
    
    (map-set tags
      { tag-id: tag-id }
      (merge tag {
        used: true,
        used-at: (some stacks-block-height)
      }))
    (ok true))
)

;; Revoke permit
(define-public (revoke-permit (permit-id uint))
  (let
    ((permit (unwrap! (map-get? permits { permit-id: permit-id }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set permits
      { permit-id: permit-id }
      (merge permit { valid: false }))
    
    (map-set hunter-permits
      { hunter: (get hunter permit) }
      { permit-id: permit-id, active: false })
    (ok true))
)

;; Renew permit
(define-public (renew-permit (permit-id uint) (duration-days uint) (fees-paid uint))
  (let
    (
      (permit (unwrap! (map-get? permits { permit-id: permit-id }) err-not-found))
      (new-expiration (+ stacks-block-height (* duration-days u144)))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set permits
      { permit-id: permit-id }
      (merge permit {
        expires-at: new-expiration,
        valid: true,
        fees-paid: (+ (get fees-paid permit) fees-paid)
      }))
    (ok true))
)
