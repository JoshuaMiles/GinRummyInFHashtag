module Cards

//Any generic card type
type Suit = Spades | Clubs | Hearts | Diamonds
type Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type Card = { suit: Suit; rank: Rank}

type Hand = Card seq
type Deck = Card seq

let AllSuits = [ Spades; Clubs; Hearts; Diamonds ]
let AllRanks = [ Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King ]

// Filter the cards until all the cards from the run are gone and than find out how many are removed repeat for each card.
 
let allCards = 
    seq { 
        for s in AllSuits do
            for r in AllRanks do
                yield {suit=s; rank=r}
    }

let FullDeck = 
    allCards

let Shuffle (deck:Deck) =
    let rand = System.Random()
    Seq.sortBy (fun _ -> rand.Next() ) deck


let cardRankSeq = seq [ Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King ]

let cardSuitSeq = seq [Hearts ; Clubs ; Diamonds ; Spades ]

let emptyHand = seq<Card>[]

let emptySeqOfCards = seq<seq<Card>>[]

type Set = seq<seq<Card>>

type Run = seq<seq<Card>>

// type setOrRunWithDeadwood = {runOrSet : seq<Card> ; deadWoodValue : int  }

type CardAndHand = {card : Card ; hand : Hand  ; set : Set ; run : Run }

let heartTwo  = {suit = Hearts ; rank = Two }

let heartThree = {suit = Hearts ; rank = Three }

let heartFour = {suit = Hearts ; rank = Four }

let heartKing =  {suit = Hearts ; rank = King }

let spadeTwo  = {suit = Spades ; rank = Two }

let spadeThree = {suit = Spades ; rank = Three } 

let spadeFour = {suit = Spades ; rank = Four }

let spadeKing =  {suit = Spades ; rank = King }

let clubTwo = {suit = Clubs ; rank = Two}

let eightHeart = {suit = Hearts ; rank = Eight}

let nineHeart = {suit = Hearts ; rank = Nine}

let threeHeart = {suit = Hearts ; rank = Three}

let fiveHeart = {suit = Hearts ; rank = Five}

let queenHeart = {suit = Hearts ; rank = Queen}

let jackHeart = {suit = Hearts ; rank = Jack}


let tenHeart = {suit = Hearts ; rank = Ten}

let tenDiamond = {suit = Diamonds ; rank = Ten}

let tenClub = {suit = Clubs ; rank = Ten}

let tenSpade = {suit = Spades; rank = Ten}

let eightSpade = {suit = Spades; rank = Eight}

let nineSpade = {suit = Spades; rank = Nine}


let sixHeart  = {suit = Hearts; rank = Six}

let sevenHeart  = {suit = Hearts; rank = Seven}

let testHand = seq [ eightHeart ; nineHeart ; threeHeart ; heartTwo ; heartFour ; fiveHeart ; sixHeart; sevenHeart ; queenHeart ; tenHeart ; tenDiamond ; tenClub ; tenSpade ; spadeTwo ; clubTwo ]

let testRun = seq [eightHeart ; nineHeart ; tenHeart]

//let testCardAndHandWithSet = { card = queenHeart; hand =  testHand ; set = seq[seq[tenDiamond;tenClub;tenHeart]] ; run = seq [seq[eightHeart; nineHeart; tenHeart]; seq[spadeTwo;spadeThree;spadeFour]] }

    // Fixme: change so that it returns a shuffled deck


// Add other functions here related to Card Games ...