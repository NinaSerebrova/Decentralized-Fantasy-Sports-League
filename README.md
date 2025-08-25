# 🏆 Decentralized Fantasy Sports League

A blockchain-powered fantasy sports platform built on Stacks that enables fair, transparent, and automated league management with smart contract-driven prize distribution.

## ✨ Features

- 🎯 **League Creation**: Create custom fantasy leagues with configurable buy-ins and team limits
- 👥 **Team Management**: Join leagues, build rosters, and manage your fantasy teams
- 📊 **Player Database**: Comprehensive player management with real-world team affiliations
- 🏅 **Automated Rankings**: Smart contract-based scoring and ranking system
- 💰 **Prize Distribution**: Automatic STX distribution to winners (60% / 30% / 10% split)
- 🔒 **Decentralized**: No central authority - fully governed by smart contracts

## 🚀 Quick Start

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet with STX for transactions

### Installation

```bash
git clone <repository-url>
cd Decentralized-Fantasy-Sports-League
clarinet check
```

## 📖 Usage Guide

### 1. 🏟️ Creating a League

```clarity
(contract-call? .Fantasy create-league "My League" u5000000 u8 u1000)
```

- **name**: League name (max 50 characters)
- **buy-in**: Entry fee in microSTX (minimum 1 STX)
- **max-teams**: Maximum teams allowed (2-12)
- **season-length**: Duration in blocks

### 2. 🏈 Joining a League

```clarity
(contract-call? .Fantasy join-league u1 "My Team")
```

- **league-id**: ID of the league to join
- **team-name**: Your team name (max 30 characters)

### 3. 👨‍💼 Managing Players

Only contract owner can add players:

```clarity
(contract-call? .Fantasy create-player "Tom Brady" "QB" "Buccaneers")
```

### 4. 📋 Building Your Roster

```clarity
(contract-call? .Fantasy add-player-to-roster u1 u5)
```

- **team-id**: Your team ID
- **player-id**: Player to add (max 15 players per team)

### 5. 🎮 League Operations

Start a league (creator only):
```clarity
(contract-call? .Fantasy start-league u1)
```

End league and distribute prizes:
```clarity
(contract-call? .Fantasy end-league-and-distribute u1)
```

## 📊 Read-Only Functions

### Get League Information
```clarity
(contract-call? .Fantasy get-league u1)
```

### Check Team Details
```clarity
(contract-call? .Fantasy get-team u1)
```

### View Player Stats
```clarity
(contract-call? .Fantasy get-player u1)
```

### Calculate Team Score
```clarity
(contract-call? .Fantasy get-team-score u1)
```

### Check Roster
```clarity
(contract-call? .Fantasy is-player-in-roster u1 u5)
```

## 💎 Prize Distribution

- 🥇 **1st Place**: 60% of prize pool
- 🥈 **2nd Place**: 30% of prize pool  
- 🥉 **3rd Place**: 10% of prize pool

## 🔧 Contract Constants

- **MAX_TEAMS_PER_LEAGUE**: 12 teams maximum
- **MAX_PLAYERS_PER_TEAM**: 15 players maximum
- **MIN_BUY_IN**: 1 STX minimum entry fee

## 🛡️ Security Features

- ✅ Owner-only player management
- ✅ League creator controls
- ✅ Automatic STX escrow
- ✅ Duplicate player prevention
- ✅ Roster size validation

## 📝 League Status Flow

1. **"open"** - Accepting new teams
2. **"active"** - Season in progress
3. **"completed"** - Prizes distributed

## 🧪 Testing

```bash
clarinet test
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run tests
5. Submit a pull request

## 📜 License

This project is open source and available under the MIT License.

## 🆘 Support

Having issues? Check out the [Stacks documentation](https://docs.stacks.co/) or open an issue on GitHub.

---

Built with ❤️ on Stacks blockchain
