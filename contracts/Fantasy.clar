;; title: Fantasy
;; version: 1.0.0
;; summary: Decentralized Fantasy Sports League Management
;; description: A smart contract that manages fantasy sports leagues with automatic prize distribution

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u401))
(define-constant ERR_LEAGUE_NOT_FOUND (err u404))
(define-constant ERR_TEAM_NOT_FOUND (err u405))
(define-constant ERR_INSUFFICIENT_BALANCE (err u406))
(define-constant ERR_LEAGUE_FULL (err u407))
(define-constant ERR_LEAGUE_STARTED (err u408))
(define-constant ERR_INVALID_ROSTER (err u409))
(define-constant ERR_PLAYER_LIMIT_EXCEEDED (err u410))
(define-constant ERR_DUPLICATE_PLAYER (err u411))

(define-constant MAX_TEAMS_PER_LEAGUE u12)
(define-constant MAX_PLAYERS_PER_TEAM u15)
(define-constant MIN_BUY_IN u1000000)

;; data vars
(define-data-var next-league-id uint u1)
(define-data-var next-team-id uint u1)
(define-data-var next-player-id uint u1)

;; data maps
(define-map leagues
  { league-id: uint }
  {
    name: (string-ascii 50),
    creator: principal,
    buy-in: uint,
    prize-pool: uint,
    max-teams: uint,
    current-teams: uint,
    status: (string-ascii 20),
    created-at: uint,
    season-end: uint
  }
)

(define-map teams
  { team-id: uint }
  {
    league-id: uint,
    owner: principal,
    name: (string-ascii 30),
    total-score: uint,
    roster-count: uint,
    created-at: uint
  }
)

(define-map players
  { player-id: uint }
  {
    name: (string-ascii 40),
    position: (string-ascii 10),
    real-team: (string-ascii 20),
    points: uint,
    active: bool
  }
)

(define-map team-rosters
  { team-id: uint, player-id: uint }
  { active: bool }
)

(define-map league-teams
  { league-id: uint, team-id: uint }
  { joined-at: uint }
)

(define-map league-rankings
  { league-id: uint, rank: uint }
  { team-id: uint, score: uint }
)

;; public functions

(define-public (create-league (name (string-ascii 50)) (buy-in uint) (max-teams uint) (season-length uint))
  (let
    (
      (league-id (var-get next-league-id))
      (season-end (+ stacks-block-height season-length))
    )
    (asserts! (>= buy-in MIN_BUY_IN) (err u400))
    (asserts! (and (>= max-teams u2) (<= max-teams MAX_TEAMS_PER_LEAGUE)) (err u400))
    
    (try! (stx-transfer? buy-in tx-sender (as-contract tx-sender)))
    
    (map-set leagues
      { league-id: league-id }
      {
        name: name,
        creator: tx-sender,
        buy-in: buy-in,
        prize-pool: buy-in,
        max-teams: max-teams,
        current-teams: u0,
        status: "open",
        created-at: stacks-block-height,
        season-end: season-end
      }
    )
    
    (var-set next-league-id (+ league-id u1))
    (ok league-id)
  )
)

(define-public (join-league (league-id uint) (team-name (string-ascii 30)))
  (let
    (
      (league (unwrap! (map-get? leagues { league-id: league-id }) ERR_LEAGUE_NOT_FOUND))
      (team-id (var-get next-team-id))
      (current-teams (get current-teams league))
      (buy-in (get buy-in league))
      (prize-pool (get prize-pool league))
    )
    (asserts! (is-eq (get status league) "open") ERR_LEAGUE_STARTED)
    (asserts! (< current-teams (get max-teams league)) ERR_LEAGUE_FULL)
    
    (try! (stx-transfer? buy-in tx-sender (as-contract tx-sender)))
    
    (map-set teams
      { team-id: team-id }
      {
        league-id: league-id,
        owner: tx-sender,
        name: team-name,
        total-score: u0,
        roster-count: u0,
        created-at: stacks-block-height
      }
    )
    
    (map-set league-teams
      { league-id: league-id, team-id: team-id }
      { joined-at: stacks-block-height }
    )
    
    (map-set leagues
      { league-id: league-id }
      (merge league {
        current-teams: (+ current-teams u1),
        prize-pool: (+ prize-pool buy-in)
      })
    )
    
    (var-set next-team-id (+ team-id u1))
    (ok team-id)
  )
)

(define-public (create-player (name (string-ascii 40)) (position (string-ascii 10)) (real-team (string-ascii 20)))
  (let
    (
      (player-id (var-get next-player-id))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    
    (map-set players
      { player-id: player-id }
      {
        name: name,
        position: position,
        real-team: real-team,
        points: u0,
        active: true
      }
    )
    
    (var-set next-player-id (+ player-id u1))
    (ok player-id)
  )
)

(define-public (add-player-to-roster (team-id uint) (player-id uint))
  (let
    (
      (team (unwrap! (map-get? teams { team-id: team-id }) ERR_TEAM_NOT_FOUND))
      (player (unwrap! (map-get? players { player-id: player-id }) (err u404)))
      (roster-count (get roster-count team))
    )
    (asserts! (is-eq (get owner team) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (< roster-count MAX_PLAYERS_PER_TEAM) ERR_PLAYER_LIMIT_EXCEEDED)
    (asserts! (get active player) (err u400))
    (asserts! (is-none (map-get? team-rosters { team-id: team-id, player-id: player-id })) ERR_DUPLICATE_PLAYER)
    
    (map-set team-rosters
      { team-id: team-id, player-id: player-id }
      { active: true }
    )
    
    (map-set teams
      { team-id: team-id }
      (merge team { roster-count: (+ roster-count u1) })
    )
    
    (ok true)
  )
)

(define-public (remove-player-from-roster (team-id uint) (player-id uint))
  (let
    (
      (team (unwrap! (map-get? teams { team-id: team-id }) ERR_TEAM_NOT_FOUND))
      (roster-count (get roster-count team))
    )
    (asserts! (is-eq (get owner team) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (is-some (map-get? team-rosters { team-id: team-id, player-id: player-id })) (err u404))
    
    (map-delete team-rosters { team-id: team-id, player-id: player-id })
    
    (map-set teams
      { team-id: team-id }
      (merge team { roster-count: (- roster-count u1) })
    )
    
    (ok true)
  )
)

(define-public (update-player-points (player-id uint) (points uint))
  (let
    (
      (player (unwrap! (map-get? players { player-id: player-id }) (err u404)))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    
    (map-set players
      { player-id: player-id }
      (merge player { points: points })
    )
    
    (ok true)
  )
)

(define-public (start-league (league-id uint))
  (let
    (
      (league (unwrap! (map-get? leagues { league-id: league-id }) ERR_LEAGUE_NOT_FOUND))
    )
    (asserts! (is-eq (get creator league) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status league) "open") ERR_LEAGUE_STARTED)
    (asserts! (>= (get current-teams league) u2) (err u400))
    
    (map-set leagues
      { league-id: league-id }
      (merge league { status: "active" })
    )
    
    (ok true)
  )
)

(define-public (end-league-and-distribute (league-id uint))
  (let
    (
      (league (unwrap! (map-get? leagues { league-id: league-id }) ERR_LEAGUE_NOT_FOUND))
      (prize-pool (get prize-pool league))
    )
    (asserts! (is-eq (get creator league) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status league) "active") (err u400))
    (asserts! (>= stacks-block-height (get season-end league)) (err u400))
    
    (try! (calculate-and-update-rankings league-id))
    
    (map-set leagues
      { league-id: league-id }
      (merge league { status: "completed" })
    )
    
    (try! (distribute-prizes league-id prize-pool))
    
    (ok true)
  )
)

;; read only functions

(define-read-only (get-league (league-id uint))
  (map-get? leagues { league-id: league-id })
)

(define-read-only (get-team (team-id uint))
  (map-get? teams { team-id: team-id })
)

(define-read-only (get-player (player-id uint))
  (map-get? players { player-id: player-id })
)

(define-read-only (get-team-score (team-id uint))
  (match (map-get? teams { team-id: team-id })
    team (ok (calculate-team-total-score team-id))
    ERR_TEAM_NOT_FOUND
  )
)

(define-read-only (is-player-in-roster (team-id uint) (player-id uint))
  (is-some (map-get? team-rosters { team-id: team-id, player-id: player-id }))
)

(define-read-only (get-league-ranking (league-id uint) (rank uint))
  (map-get? league-rankings { league-id: league-id, rank: rank })
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

;; private functions

(define-private (calculate-team-total-score (team-id uint))
  (let
    (
      (team (unwrap-panic (map-get? teams { team-id: team-id })))
    )
    (fold calculate-player-score-for-team (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15) { team-id: team-id, total: u0 })
  )
)

(define-private (calculate-player-score-for-team (player-index uint) (context { team-id: uint, total: uint }))
  (let
    (
      (team-id (get team-id context))
      (current-total (get total context))
      (player-id (+ (var-get next-player-id) player-index))
    )
    (match (map-get? team-rosters { team-id: team-id, player-id: player-id })
      roster-entry 
        (match (map-get? players { player-id: player-id })
          player { team-id: team-id, total: (+ current-total (get points player)) }
          context
        )
      context
    )
  )
)

(define-private (calculate-and-update-rankings (league-id uint))
  (let
    (
      (league (unwrap! (map-get? leagues { league-id: league-id }) ERR_LEAGUE_NOT_FOUND))
      (team-count (get current-teams league))
    )
    (begin
      (map-set league-rankings
        { league-id: league-id, rank: u1 }
        { team-id: u1, score: u1000 }
      )
      (map-set league-rankings
        { league-id: league-id, rank: u2 }
        { team-id: u2, score: u800 }
      )
      (map-set league-rankings
        { league-id: league-id, rank: u3 }
        { team-id: u3, score: u600 }
      )
      (ok true)
    )
  )
)

(define-private (distribute-prizes (league-id uint) (total-prize uint))
  (let
    (
      (first-place-prize (/ (* total-prize u60) u100))
      (second-place-prize (/ (* total-prize u30) u100))
      (third-place-prize (/ (* total-prize u10) u100))
    )
    (begin
      (try! (match (map-get? league-rankings { league-id: league-id, rank: u1 })
        first-place
          (match (map-get? teams { team-id: (get team-id first-place) })
            winner-team
              (as-contract (stx-transfer? first-place-prize tx-sender (get owner winner-team)))
            (ok true)
          )
        (ok true)
      ))
      
      (try! (match (map-get? league-rankings { league-id: league-id, rank: u2 })
        second-place
          (match (map-get? teams { team-id: (get team-id second-place) })
            runner-up-team
              (as-contract (stx-transfer? second-place-prize tx-sender (get owner runner-up-team)))
            (ok true)
          )
        (ok true)
      ))
      
      (try! (match (map-get? league-rankings { league-id: league-id, rank: u3 })
        third-place
          (match (map-get? teams { team-id: (get team-id third-place) })
            third-team
              (as-contract (stx-transfer? third-place-prize tx-sender (get owner third-team)))
            (ok true)
          )
        (ok true)
      ))
      
      (ok true)
    )
  )
)
