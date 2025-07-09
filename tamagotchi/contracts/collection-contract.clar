;; Virtual pets that evolve based on feeding schedule, Ethereum gas prices, and social interactions

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-INVALID-PRICE (err u104))
(define-constant ERR-TRANSFER-FAILED (err u105))

;; Contract owner
(define-constant PET-MASTER tx-sender)

;; Pet NFT definition
(define-non-fungible-token meta-pet uint)

;; Data variables
(define-data-var next-pet-id uint u1)
(define-data-var last-gas-price uint u20) ;; Starting gas price in gwei
(define-data-var adoption-fee uint u250) ;; 2.5% fee (250 basis points)

;; Pet metadata structure
(define-map pet-metadata uint {
    breed: (string-ascii 64),
    description: (string-ascii 256),
    image-uri: (string-ascii 256),
    experience-level: uint,
    birth-block: uint,
    last-fed: uint,
    happiness-score: uint
})

;; Pet shelter listings
(define-map shelter-listings uint {
    trainer: principal,
    price: uint,
    listed-at: uint
})

;; Trainer behavior tracking
(define-map trainer-stats principal {
    total-pets: uint,
    total-feedings: uint,
    last-activity: uint,
    trainer-bond: uint
})

;; Experience thresholds
(define-map experience-thresholds uint {
    gas-threshold: uint,
    activity-threshold: uint,
    care-threshold: uint
})

;; Initialize experience thresholds
(map-set experience-thresholds u1 {gas-threshold: u15, activity-threshold: u100, care-threshold: u50})
(map-set experience-thresholds u2 {gas-threshold: u25, activity-threshold: u200, care-threshold: u100})
(map-set experience-thresholds u3 {gas-threshold: u35, activity-threshold: u300, care-threshold: u200})

;; Read-only functions

;; Get pet metadata
(define-read-only (get-pet-metadata (pet-id uint))
    (map-get? pet-metadata pet-id)
)

;; Get shelter listing
(define-read-only (get-shelter-listing (pet-id uint))
    (map-get? shelter-listings pet-id)
)

;; Get trainer stats
(define-read-only (get-trainer-stats (trainer principal))
    (map-get? trainer-stats trainer)
)

;; Get pet owner
(define-read-only (get-pet-owner (pet-id uint))
    (nft-get-owner? meta-pet pet-id)
)

;; Calculate experience level based on various factors
(define-read-only (calculate-experience-level (pet-id uint))
    (let ((metadata (unwrap! (get-pet-metadata pet-id) u0))
          (current-gas (var-get last-gas-price))
          (current-block block-height)
          (birth-block (get birth-block metadata))
          (happiness-score (get happiness-score metadata)))
        (let ((block-age (- current-block birth-block))
              (gas-factor (if (> current-gas u30) u2 u1))
              (age-factor (if (> block-age u1000) u2 u1))
              (happiness-factor (if (> happiness-score u100) u2 u1)))
            (+ gas-factor age-factor happiness-factor)
        )
    )
)

;; Get current adoption fee
(define-read-only (get-adoption-fee)
    (var-get adoption-fee)
)

;; Public functions

;; Hatch new meta pet
(define-public (hatch-pet (breed (string-ascii 64)) (description (string-ascii 256)) (image-uri (string-ascii 256)))
    (let ((pet-id (var-get next-pet-id)))
        (try! (nft-mint? meta-pet pet-id tx-sender))
        (map-set pet-metadata pet-id {
            breed: breed,
            description: description,
            image-uri: image-uri,
            experience-level: u1,
            birth-block: block-height,
            last-fed: block-height,
            happiness-score: u0
        })
        (update-trainer-stats tx-sender u1 u1)
        (var-set next-pet-id (+ pet-id u1))
        (ok pet-id)
    )
)

;; List pet in shelter
(define-public (list-in-shelter (pet-id uint) (price uint))
    (let ((owner (unwrap! (nft-get-owner? meta-pet pet-id) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (asserts! (> price u0) ERR-INVALID-PRICE)
        (map-set shelter-listings pet-id {
            trainer: tx-sender,
            price: price,
            listed-at: block-height
        })
        (ok true)
    )
)

;; Remove pet from shelter
(define-public (remove-from-shelter (pet-id uint))
    (let ((listing (unwrap! (get-shelter-listing pet-id) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get trainer listing)) ERR-NOT-AUTHORIZED)
        (map-delete shelter-listings pet-id)
        (ok true)
    )
)

;; Adopt pet from shelter
(define-public (adopt-pet (pet-id uint))
    (let ((listing (unwrap! (get-shelter-listing pet-id) ERR-NOT-FOUND))
          (price (get price listing))
          (seller (get trainer listing))
          (fee (/ (* price (var-get adoption-fee)) u10000)))
        (try! (stx-transfer? (- price fee) tx-sender seller))
        (try! (stx-transfer? fee tx-sender PET-MASTER))
        (try! (nft-transfer? meta-pet pet-id seller tx-sender))
        (map-delete shelter-listings pet-id)
        (update-trainer-stats tx-sender u1 u1)
        (update-trainer-stats seller u0 u1)
        (update-pet-interaction pet-id)
        (ok true)
    )
)

;; Transfer pet (updates trainer behavior)
(define-public (transfer-pet (pet-id uint) (recipient principal))
    (let ((owner (unwrap! (nft-get-owner? meta-pet pet-id) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (try! (nft-transfer? meta-pet pet-id tx-sender recipient))
        (update-trainer-stats tx-sender u0 u1)
        (update-trainer-stats recipient u1 u1)
        (update-pet-interaction pet-id)
        (ok true)
    )
)

;; Update gas price (can be called by oracle or admin)
(define-public (update-gas-price (new-price uint))
    (begin
        (asserts! (is-eq tx-sender PET-MASTER) ERR-NOT-AUTHORIZED)
        (var-set last-gas-price new-price)
        (ok true)
    )
)

;; Level up pet based on current conditions
(define-public (level-up-pet (pet-id uint))
    (let ((metadata (unwrap! (get-pet-metadata pet-id) ERR-NOT-FOUND))
          (owner (unwrap! (nft-get-owner? meta-pet pet-id) ERR-NOT-FOUND))
          (new-experience-level (calculate-experience-level pet-id)))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (map-set pet-metadata pet-id (merge metadata {
            experience-level: new-experience-level,
            last-fed: block-height
        }))
        (update-trainer-stats tx-sender u0 u1)
        (ok new-experience-level)
    )
)

;; Feed pet (increases happiness score)
(define-public (feed-pet (pet-id uint))
    (let ((metadata (unwrap! (get-pet-metadata pet-id) ERR-NOT-FOUND))
          (owner (unwrap! (nft-get-owner? meta-pet pet-id) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (map-set pet-metadata pet-id (merge metadata {
            last-fed: block-height,
            happiness-score: (+ (get happiness-score metadata) u10)
        }))
        (update-trainer-stats tx-sender u0 u1)
        (ok true)
    )
)

;; Private functions

;; Update trainer statistics
(define-private (update-trainer-stats (trainer principal) (pets-change uint) (activity-increment uint))
    (let ((current-stats (default-to {total-pets: u0, total-feedings: u0, last-activity: u0, trainer-bond: u0} 
                                   (get-trainer-stats trainer))))
        (map-set trainer-stats trainer {
            total-pets: (+ (get total-pets current-stats) pets-change),
            total-feedings: (+ (get total-feedings current-stats) activity-increment),
            last-activity: block-height,
            trainer-bond: (+ (get trainer-bond current-stats) (* activity-increment u5))
        })
    )
)

;; Update pet interaction timestamp
(define-private (update-pet-interaction (pet-id uint))
    (let ((metadata (unwrap! (get-pet-metadata pet-id) false)))
        (map-set pet-metadata pet-id (merge metadata {
            last-fed: block-height
        }))
        true
    )
)

;; Admin functions

;; Update adoption fee (only pet master)
(define-public (set-adoption-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender PET-MASTER) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-fee u1000) ERR-INVALID-PRICE) ;; Max 10% fee
        (var-set adoption-fee new-fee)
        (ok true)
    )
)