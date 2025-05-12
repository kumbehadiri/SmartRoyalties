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

(define-map work-categories
    principal
    (list 10 (string-ascii 20))
)

(define-public (add-work-categories (categories (list 10 (string-ascii 20))))
    (let ((work (unwrap! (map-get? creative-works tx-sender) err-not-found)))
        (ok (map-set work-categories tx-sender categories))
    )
)

(define-read-only (get-work-categories (owner principal))
    (ok (map-get? work-categories owner))
)


(define-map pricing-tiers
    principal
    {
        basic: uint,
        premium: uint,
        enterprise: uint
    }
)

(define-public (set-work-pricing (basic uint) (premium uint) (enterprise uint))
    (let ((work (unwrap! (map-get? creative-works tx-sender) err-not-found)))
        (ok (map-set pricing-tiers tx-sender {
            basic: basic,
            premium: premium,
            enterprise: enterprise
        }))
    )
)

(define-public (pay-tiered-royalty (work-owner principal) (tier (string-ascii 10)))
    (let (
        (prices (unwrap! (map-get? pricing-tiers work-owner) err-not-found))
        (amount (if (is-eq tier "basic")
            (get basic prices)
            (if (is-eq tier "premium")
                (get premium prices)
                (get enterprise prices))))
    )
        (try! (pay-royalty work-owner amount))
        (ok true)
    )
)


(define-map revenue-pools
    (string-ascii 50)
    {
        members: (list 50 principal),
        share-percentages: (list 50 uint),
        total-earnings: uint
    }
)

(define-public (create-revenue-pool (pool-name (string-ascii 50)) (members (list 50 principal)) (percentages (list 50 uint)))
    (ok (map-set revenue-pools pool-name {
        members: members,
        share-percentages: percentages,
        total-earnings: u0
    }))
)

(define-public (distribute-pool-earnings (pool-name (string-ascii 50)) (amount uint))
    (let ((pool (unwrap! (map-get? revenue-pools pool-name) err-not-found)))
        (try! (stx-transfer? amount tx-sender contract-owner))
        (ok (map-set revenue-pools pool-name 
            (merge pool {total-earnings: (+ (get total-earnings pool) amount)})))
    )
)


(define-map work-versions
    { owner: principal, version: uint }
    {
        hash: (string-ascii 64),
        timestamp: uint,
        changes: (string-ascii 200)
    }
)

(define-public (add-work-version (hash (string-ascii 64)) (changes (string-ascii 200)))
    (let (
        (work (unwrap! (map-get? creative-works tx-sender) err-not-found))
        (version-count (unwrap! (get-last-version tx-sender) err-not-found))
    )
        (ok (map-set work-versions 
            {owner: tx-sender, version: (+ version-count u1)}
            {hash: hash, timestamp: stacks-block-height, changes: changes}))
    )
)

(define-read-only (get-last-version (owner principal))
    (ok (default-to u0 (get timestamp (map-get? work-versions {owner: owner, version: u1}))))
)


(define-map subscriptions
    { subscriber: principal, creator: principal }
    {
        start-height: uint,
        end-height: uint,
        subscription-type: (string-ascii 10)
    }
)

(define-public (create-subscription (creator principal) (duration uint) (sub-type (string-ascii 10)))
    (ok (map-set subscriptions 
        {subscriber: tx-sender, creator: creator}
        {
            start-height: stacks-block-height,
            end-height: (+ stacks-block-height duration),
            subscription-type: sub-type
        }))
)

(define-read-only (check-subscription (subscriber principal) (creator principal))
    (let ((sub (map-get? subscriptions {subscriber: subscriber, creator: creator})))
        (ok (and (is-some sub)
            (< stacks-block-height (get end-height (unwrap-panic sub)))))
    )
)


(define-map collaborations
    (string-ascii 50)
    {
        creators: (list 10 principal),
        split-percentages: (list 10 uint),
        work-status: (string-ascii 10)
    }
)

(define-public (create-collaboration (collab-id (string-ascii 50)) (creators (list 10 principal)) (splits (list 10 uint)))
    (ok (map-set collaborations collab-id {
        creators: creators,
        split-percentages: splits,
        work-status: "active"
    }))
)

(define-public (update-collab-status (collab-id (string-ascii 50)) (status (string-ascii 10)))
    (let ((collab (unwrap! (map-get? collaborations collab-id) err-not-found)))
        (ok (map-set collaborations collab-id 
            (merge collab {work-status: status})))
    )
)

(define-map work-analytics
    principal
    {
        views: uint,
        unique-users: uint,
        peak-earnings: uint,
        last-updated: uint
    }
)

(define-public (update-analytics (work-owner principal))
    (let (
        (current-stats (default-to 
            {views: u0, unique-users: u0, peak-earnings: u0, last-updated: u0} 
            (map-get? work-analytics work-owner)))
    )
        (ok (map-set work-analytics work-owner
            (merge current-stats {
                views: (+ (get views current-stats) u1),
                last-updated: stacks-block-height
            })))
    )
)

(define-read-only (get-work-analytics (owner principal))
    (ok (map-get? work-analytics owner))
)


(define-map promotional-discounts
    { owner: principal, promo-id: uint }
    {
        start-height: uint,
        end-height: uint,
        discount-percentage: uint,
        active: bool
    }
)

(define-data-var promo-counter uint u0)

(define-public (create-promotion (duration uint) (discount uint))
    (let
        ((work (unwrap! (map-get? creative-works tx-sender) err-not-found))
         (promo-id (+ (var-get promo-counter) u1)))
        (asserts! (<= discount u1000) err-invalid-percentage)
        (var-set promo-counter promo-id)
        (ok (map-set promotional-discounts
            { owner: tx-sender, promo-id: promo-id }
            {
                start-height: stacks-block-height,
                end-height: (+ stacks-block-height duration),
                discount-percentage: discount,
                active: true
            }
        ))
    )
)

(define-read-only (get-discounted-price (owner principal) (original-price uint))
    (let
        ((promo (map-get? promotional-discounts { owner: owner, promo-id: (var-get promo-counter) })))
        (if (and 
            (is-some promo)
            (active-promotion? (unwrap-panic promo)))
            (let ((discount (get discount-percentage (unwrap-panic promo))))
                (ok (- original-price (/ (* original-price discount) u1000))))
            (ok original-price)
        )
    )
)

(define-private (active-promotion? (promo {start-height: uint, end-height: uint, discount-percentage: uint, active: bool}))
    (and
        (get active promo)
        (>= (get end-height promo) stacks-block-height)
        (<= (get start-height promo) stacks-block-height)
    )
)


(define-map work-bundles
    (string-ascii 50)
    {
        creator: principal,
        works: (list 10 principal),
        bundle-price: uint,
        active: bool
    }
)

(define-public (create-bundle (bundle-id (string-ascii 50)) (works (list 10 principal)) (price uint))
    (let
        ((work-exists (map-get? work-bundles bundle-id)))
        (asserts! (is-none work-exists) err-already-registered)
        (ok (map-set work-bundles bundle-id
            {
                creator: tx-sender,
                works: works,
                bundle-price: price,
                active: true
            }
        ))
    )
)

(define-public (purchase-bundle (bundle-id (string-ascii 50)))
    (let
        ((bundle (unwrap! (map-get? work-bundles bundle-id) err-not-found)))
        (asserts! (get active bundle) err-unauthorized)
        (try! (stx-transfer? (get bundle-price bundle) tx-sender (get creator bundle)))
        (map register-bundle-access (get works bundle))
        (ok true)
    )
)

(define-private (register-bundle-access (work-owner principal))
    (map-set usage-tracking
        {work-owner: work-owner, user: tx-sender}
        {
            last-payment: stacks-block-height,
            total-paid: u0,
            license-expiry: (+ stacks-block-height u14400)
        }
    )
)