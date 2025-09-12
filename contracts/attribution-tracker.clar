;; Creative Work Attribution & Inspiration Chain Tracker
;; Tracks creative inspirations, influences, and derivative work relationships

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-WORK-NOT-FOUND (err u101))
(define-constant ERR-INVALID-PERCENTAGE (err u102))
(define-constant ERR-ATTRIBUTION-EXISTS (err u103))
(define-constant ERR-SELF-ATTRIBUTION (err u104))
(define-constant ERR-INVALID-CHAIN (err u105))
(define-constant ERR-MAX-ATTRIBUTIONS (err u106))

;; Data variables
(define-data-var next-attribution-id uint u1)
(define-data-var max-attribution-depth uint u5)
(define-data-var min-attribution-percentage uint u10) ;; 1% minimum

;; Core attribution relationships
(define-map creative-attributions
    { attribution-id: uint }
    {
        derivative-work: principal,
        inspiration-work: principal,
        attribution-percentage: uint,
        attribution-type: (string-ascii 20), ;; "direct", "indirect", "style", "technique"
        description: (string-ascii 100),
        verified: bool,
        timestamp: uint
    }
)

;; Inspiration networks for each work
(define-map work-inspirations
    { work: principal }
    {
        inspiration-count: uint,
        total-attribution-given: uint,
        influence-score: uint,
        last-updated: uint
    }
)

;; Derivative tracking for original works
(define-map work-derivatives
    { original-work: principal }
    {
        derivative-count: uint,
        total-attribution-received: uint,
        influence-reach: uint,
        revenue-shared: uint
    }
)


;; Revenue sharing records
(define-map attribution-payments
    { payer: principal, recipient: principal, attribution-id: uint }
    {
        amount: uint,
        payment-date: uint,
        revenue-source: (string-ascii 30)
    }
)

;; Register inspiration attribution for a work
(define-public (register-attribution 
    (inspiration-work principal)
    (attribution-percentage uint)
    (attribution-type (string-ascii 20))
    (description (string-ascii 100)))
    
    (let ((attribution-id (var-get next-attribution-id))
          (current-inspirations (default-to 
              { inspiration-count: u0, total-attribution-given: u0, influence-score: u0, last-updated: u0 }
              (map-get? work-inspirations { work: tx-sender }))))
        
        (asserts! (not (is-eq tx-sender inspiration-work)) ERR-SELF-ATTRIBUTION)
        (asserts! (>= attribution-percentage (var-get min-attribution-percentage)) ERR-INVALID-PERCENTAGE)
        (asserts! (<= attribution-percentage u1000) ERR-INVALID-PERCENTAGE) ;; Max 100%
        (asserts! (<= (+ (get total-attribution-given current-inspirations) attribution-percentage) u1000) ERR-INVALID-PERCENTAGE)
        (asserts! (< (get inspiration-count current-inspirations) u10) ERR-MAX-ATTRIBUTIONS)
        
        ;; Record attribution
        (map-set creative-attributions
            { attribution-id: attribution-id }
            {
                derivative-work: tx-sender,
                inspiration-work: inspiration-work,
                attribution-percentage: attribution-percentage,
                attribution-type: attribution-type,
                description: description,
                verified: false,
                timestamp: stacks-block-height
            }
        )
        
        ;; Update work inspirations
        (map-set work-inspirations
            { work: tx-sender }
            {
                inspiration-count: (+ (get inspiration-count current-inspirations) u1),
                total-attribution-given: (+ (get total-attribution-given current-inspirations) attribution-percentage),
                influence-score: (get influence-score current-inspirations),
                last-updated: stacks-block-height
            }
        )
        
        ;; Update derivatives for inspiration work
        (let ((current-derivatives (default-to
                { derivative-count: u0, total-attribution-received: u0, influence-reach: u0, revenue-shared: u0 }
                (map-get? work-derivatives { original-work: inspiration-work }))))
            
            (map-set work-derivatives
                { original-work: inspiration-work }
                {
                    derivative-count: (+ (get derivative-count current-derivatives) u1),
                    total-attribution-received: (+ (get total-attribution-received current-derivatives) attribution-percentage),
                    influence-reach: (+ (get influence-reach current-derivatives) u1),
                    revenue-shared: (get revenue-shared current-derivatives)
                }
            )
        )
        
        (var-set next-attribution-id (+ attribution-id u1))
        (ok attribution-id)
    )
)

;; Verify attribution relationship (by inspiration work owner)
(define-public (verify-attribution (attribution-id uint) (verified bool))
    (let ((attribution (unwrap! (map-get? creative-attributions { attribution-id: attribution-id }) ERR-ATTRIBUTION-EXISTS)))
        
        (asserts! (is-eq tx-sender (get inspiration-work attribution)) ERR-NOT-AUTHORIZED)
        
        (map-set creative-attributions
            { attribution-id: attribution-id }
            (merge attribution { verified: verified })
        )
        (ok verified)
    )
)

;; Pay attribution royalties to inspiration works
(define-public (pay-attribution-royalties (payment-amount uint))
    (let ((inspirations (map-get? work-inspirations { work: tx-sender })))
        
        (if (is-some inspirations)
            (let ((inspiration-data (unwrap-panic inspirations))
                  (total-attribution (get total-attribution-given inspiration-data)))
                
                (if (> total-attribution u0)
                    (let ((attribution-payment (/ (* payment-amount total-attribution) u1000)))
                        ;; Process attribution payments (simplified)
                        (unwrap! (distribute-attribution-payments tx-sender attribution-payment) ERR-INVALID-PERCENTAGE)
                        (ok attribution-payment)
                    )
                    (ok u0)
                )
            )
            (ok u0)
        )
    )
)


;; Helper function to distribute attribution payments
(define-private (distribute-attribution-payments (derivative-work principal) (total-amount uint))
    ;; Simplified implementation - would iterate through attributions and distribute payments
    (begin
        ;; Record payment transaction
        (map-set attribution-payments
            { payer: derivative-work, recipient: derivative-work, attribution-id: u1 }
            {
                amount: total-amount,
                payment-date: stacks-block-height,
                revenue-source: "attribution-royalty"
            }
        )
        (ok total-amount)
    )
)

;; Update influence scores based on network activity
(define-public (update-influence-score (work-owner principal))
    (let ((derivatives (map-get? work-derivatives { original-work: work-owner }))
          (inspirations (map-get? work-inspirations { work: work-owner })))
        
        (if (is-some derivatives)
            (let ((derivative-data (unwrap-panic derivatives))
                  (influence-score (+ (get derivative-count derivative-data) (get influence-reach derivative-data))))
                
                (map-set work-derivatives
                    { original-work: work-owner }
                    (merge derivative-data { influence-reach: influence-score })
                )
                (ok influence-score)
            )
            (ok u0)
        )
    )
)

;; Read-only functions
(define-read-only (get-attribution (attribution-id uint))
    (map-get? creative-attributions { attribution-id: attribution-id })
)

(define-read-only (get-work-inspirations (work principal))
    (map-get? work-inspirations { work: work })
)

(define-read-only (get-work-derivatives (original-work principal))
    (map-get? work-derivatives { original-work: original-work })
)


(define-read-only (get-attribution-payment (payer principal) (recipient principal) (attribution-id uint))
    (map-get? attribution-payments { payer: payer, recipient: recipient, attribution-id: attribution-id })
)

;; Calculate influence network depth for a work
(define-read-only (calculate-influence-depth (work principal))
    (let ((inspirations (map-get? work-inspirations { work: work }))
          (derivatives (map-get? work-derivatives { original-work: work })))
        {
            inspiration-depth: (if (is-some inspirations) (get inspiration-count (unwrap-panic inspirations)) u0),
            derivative-depth: (if (is-some derivatives) (get derivative-count (unwrap-panic derivatives)) u0),
            total-influence: (+ 
                (if (is-some inspirations) (get influence-score (unwrap-panic inspirations)) u0)
                (if (is-some derivatives) (get influence-reach (unwrap-panic derivatives)) u0)
            )
        }
    )
)

;; Check if two works are connected in the attribution network
(define-read-only (check-attribution-connection (work-a principal) (work-b principal))
    (let ((inspirations-a (map-get? work-inspirations { work: work-a }))
          (derivatives-b (map-get? work-derivatives { original-work: work-b })))
        
        ;; Simplified check - would implement full network traversal in production
        (or 
            (and (is-some inspirations-a) (> (get inspiration-count (unwrap-panic inspirations-a)) u0))
            (and (is-some derivatives-b) (> (get derivative-count (unwrap-panic derivatives-b)) u0))
        )
    )
)
