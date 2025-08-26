;; LocalCommunityDAO.clar
;; A neighborhood governance platform for local decision making

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-VOTED (err u102))

(define-data-var proposal-counter uint u0)

(define-map proposals 
  uint
  {description: (string-ascii 100),
   yes-votes: uint,
   no-votes: uint,
   creator: principal,
   open: bool})

(define-map votes
  {proposal-id: uint, voter: principal}
  {choice: bool})

;; Add a new proposal
(define-public (create-proposal (description (string-ascii 100)))
  (let ((proposal-id (+ (var-get proposal-counter) u1)))
    (begin
      (var-set proposal-counter proposal-id)
      (map-set proposals
        proposal-id
        {description: description,
         yes-votes: u0,
         no-votes: u0,
         creator: tx-sender,
         open: true})
      (ok proposal-id))))

;; Vote on a proposal
(define-public (vote (proposal-id uint) (choice bool))
  (let ((proposal (map-get? proposals proposal-id))
        (already-voted (map-get? votes {proposal-id: proposal-id, voter: tx-sender})))
    (match proposal
      p (if (is-some already-voted)
            ERR-ALREADY-VOTED
            (begin
              ;; Record vote
              (map-set votes 
                {proposal-id: proposal-id, voter: tx-sender}
                {choice: choice})
              ;; Update vote counts
              (if choice
                  (map-set proposals proposal-id
                    {description: (get description p),
                     yes-votes: (+ (get yes-votes p) u1),
                     no-votes: (get no-votes p),
                     creator: (get creator p),
                     open: (get open p)})
                  (map-set proposals proposal-id
                    {description: (get description p),
                     yes-votes: (get yes-votes p),
                     no-votes: (+ (get no-votes p) u1),
                     creator: (get creator p),
                     open: (get open p)}))
              (ok true)))
      ERR-PROPOSAL-NOT-FOUND)))

;; Close a proposal
(define-public (close-proposal (proposal-id uint))
  (let ((proposal (map-get? proposals proposal-id)))
    (match proposal
      p (if (is-eq tx-sender (get creator p))
            (begin
              (map-set proposals proposal-id
                {description: (get description p),
                 yes-votes: (get yes-votes p),
                 no-votes: (get no-votes p),
                 creator: (get creator p),
                 open: false})
              (ok true))
            ERR-NOT-AUTHORIZED)
      ERR-PROPOSAL-NOT-FOUND)))

;; Read-only functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id))

(define-read-only (get-proposal-count)
  (var-get proposal-counter))

(define-read-only (has-voted (proposal-id uint) (voter principal))
  (is-some (map-get? votes {proposal-id: proposal-id, voter: voter})))
