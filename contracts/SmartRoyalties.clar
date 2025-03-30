;; SmartRoyalties - Creative work royalty platform
;; Handles royalty distribution and rights management for creative works

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-registered (err u102))
(define-constant err-invalid-percentage (err u103))
(define-constant err-unauthorized (err u104))

;; Data Variables
(define-data-var platform-fee uint u50) ;; 5% platform fee (represented as basis points)

;; Data Maps
(define-map creative-works 
    principal 
    {
        title: (string-ascii 50),
        description: (string-ascii 200),
        creation-date: uint,
        royalty-percentage: uint,
        total-earnings: uint
    }
)

(define-map rights-holders
    {work-owner: principal, holder: principal}
    {
        percentage: uint,
        earnings: uint
    }
)

(define-map usage-tracking
    {work-owner: principal, user: principal}
    {
        last-payment: uint,
        total-paid: uint,
        license-expiry: uint
    }
)

;; Public Functions

;; Register a new creative work
(define-public (register-work (title (string-ascii 50)) (description (string-ascii 200)) (royalty-percentage uint))
    (let
        ((work-exists (get title (map-get? creative-works tx-sender))))
        (asserts! (is-none work-exists) err-already-registered)
        (asserts! (<= royalty-percentage u1000) err-invalid-percentage) ;; Max 100% (1000 basis points)
        (ok (map-set creative-works tx-sender
            {
                title: title,
                description: description,
                creation-date: stacks-block-height,
                royalty-percentage: royalty-percentage,
                total-earnings: u0
            }
        ))
    )
)

;; Add rights holder for a work
(define-public (add-rights-holder (work-owner principal) (holder principal) (percentage uint))
    (let
        ((work (map-get? creative-works work-owner)))
        (asserts! (is-eq tx-sender work-owner) err-unauthorized)
        (asserts! (is-some work) err-not-found)
        (asserts! (<= percentage u1000) err-invalid-percentage)
        (ok (map-set rights-holders {work-owner: work-owner, holder: holder}
            {
                percentage: percentage,
                earnings: u0
            }
        ))
    )
)

;; Process royalty payment
(define-public (pay-royalty (work-owner principal) (amount uint))
    (let
        ((work (unwrap! (map-get? creative-works work-owner) err-not-found))
         (platform-cut (/ (* amount (var-get platform-fee)) u1000))
         (creator-amount (- amount platform-cut)))
        
        ;; Transfer platform fee
        (try! (stx-transfer? platform-cut tx-sender contract-owner))
        
        ;; Transfer creator payment
        (try! (stx-transfer? creator-amount tx-sender work-owner))
        
        ;; Update usage tracking
        (map-set usage-tracking {work-owner: work-owner, user: tx-sender}
            {
                last-payment: stacks-block-height,
                total-paid: (+ amount (default-to u0 (get total-paid (map-get? usage-tracking {work-owner: work-owner, user: tx-sender})))),
                license-expiry: (+ stacks-block-height u1440) ;; 1 day license
            }
        )
        
        ;; Update work earnings
        (map-set creative-works work-owner
            (merge work {total-earnings: (+ (get total-earnings work) amount)})
        )
        
        (ok true)
    )
)

;; Read-only Functions

;; Get work details
(define-read-only (get-work-details (owner principal))
    (ok (map-get? creative-works owner))
)

;; Get rights holder details
(define-read-only (get-rights-holder-details (work-owner principal) (holder principal))
    (ok (map-get? rights-holders {work-owner: work-owner, holder: holder}))
)

;; Get usage details
(define-read-only (get-usage-details (work-owner principal) (user principal))
    (ok (map-get? usage-tracking {work-owner: work-owner, user: user}))
)

;; Check if license is valid
(define-read-only (is-license-valid (work-owner principal) (user principal))
    (let
        ((usage (map-get? usage-tracking {work-owner: work-owner, user: user})))
        (if (is-some usage)
            (ok (< stacks-block-height (get license-expiry (unwrap-panic usage))))
            (ok false)
        )
    )
)

;; Admin Functions

;; Update platform fee
(define-public (update-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee u1000) err-invalid-percentage)
        (ok (var-set platform-fee new-fee))
    )
)

