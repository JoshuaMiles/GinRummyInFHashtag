﻿module Cards

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

let rand = System.Random()

let Shuffle (deck:Deck) =
    Seq.sortBy (fun _ -> rand.Next() ) deck


let cardRankSeq = seq [ Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King ]

let cardSuitSeq = seq [Hearts ; Clubs ; Diamonds ; Spades ]

let emptyHand = seq<Card>[]

let emptySeqOfCards = seq<seq<Card>>[]

type Set = seq<seq<Card>>

type Run = seq<seq<Card>>

// type setOrRunWithDeadwood = {runOrSet : seq<Card> ; deadWoodValue : int  }




let CheckDuplicates cards =
    let duplicates = cards |> Seq.groupBy id |> Seq.map snd |> Seq.exists (fun s -> (Seq.length s ) > 1)
    if (duplicates) then
        raise (new System.Exception "Dupes found!")

type CardAndHand = {card : Card ; hand : Hand  ; set : Set ; run : Run }

let clubThree = {suit = Clubs ; rank = Three}

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

let diamondTwo  = {suit = Diamonds ; rank = Two}

let diamondThree  = {suit = Diamonds ; rank = Three}


let diamondFour  = {suit = Diamonds ; rank = Four}

let diamondSix  = {suit = Diamonds ; rank = Six}

let diamondEight  = {suit = Diamonds ; rank = Eight}

let diamondTen  = {suit = Diamonds ; rank = Ten}

let diamondQueen  = {suit = Diamonds ; rank = Queen}

let tenDiamond = {suit = Diamonds ; rank = Ten}

let tenClub = {suit = Clubs ; rank = Ten}

let tenSpade = {suit = Spades; rank = Ten}

let eightSpade = {suit = Spades; rank = Eight}

let nineSpade = {suit = Spades; rank = Nine}


let sixHeart  = {suit = Hearts; rank = Six}

let sevenHeart  = {suit = Hearts; rank = Seven}

let testHand = seq [ eightHeart ; nineHeart ; threeHeart ; heartTwo ; heartFour ; fiveHeart ; sixHeart; sevenHeart ; queenHeart ; tenHeart ; tenDiamond ; tenClub ; tenSpade ; spadeTwo ; clubTwo ]

let testRun = seq [eightHeart ; nineHeart ; tenHeart]

   (*
let testHand = seq [ eightHeart ;eightSpade; nineSpade  ; nineHeart ; tenHeart ; threeHeart ; heartTwo ; heartFour ; fiveHeart ; sixHeart; sevenHeart ; queenHeart ; tenDiamond ; tenClub ; tenSpade ; spadeTwo ; spadeThree ; spadeFour ; clubTwo ]
let testHandWithFullSet = seq [threeHeart ; diamondThree ; spadeThree ; clubThree]

let testHandFaulty = seq [diamondQueen;  diamondTen ; diamondEight ; diamondSix ;diamondFour;diamondTwo ; jackHeart ; nineHeart ;sevenHeart ;  sixHeart; fiveHeart  ]
let testCardAndHandFaulty = { card = fiveHeart; hand =  testHandFaulty ; set = emptySeqOfCards; run =emptySeqOfCards}

let testCardAndHandFullSet = { card = Seq.head ( testHandWithFullSet ); hand =  testHandWithFullSet ; set = emptySeqOfCards ; run = emptySeqOfCards }


let emptyHand = seq<Card>[]

let testCardAndHand = { card = Seq.head (listOfSortedSuits testHand ); hand = listOfSortedSuits Hand14 ; set = emptySeqOfCards ; run = emptySeqOfCards }

let testCardAndEmptyHandWithSetAndRun = { card = (Seq.head testHand); hand = listOfSortedSuits testHand ; set = emptySeqOfCards ; run = emptySeqOfCards  }
let testCardAndHandWithQueenAndKing = { card = jackHeart; hand = testHandWithJustQueenAndKing ; set = emptySeqOfCards ; run = emptySeqOfCards  }


let testCardAndEmptyHand = { card = queenHeart; hand =  emptyHand ; set = emptySeqOfCards  ; run = emptySeqOfCards}

let testCardAndHandSingleCard = { card = tenDiamond; hand = seq[tenDiamond] ; set = emptySeqOfCards  ; run = emptySeqOfCards}

*)

let SA  = { rank= Ace;      suit= Spades }
let S2  = { rank= Two;      suit= Spades }
let S3  = { rank= Three;    suit= Spades }
let S4  = { rank= Four;     suit= Spades }
let S5  = { rank= Five;     suit= Spades }
let S6  = { rank= Six;      suit= Spades }
let S7  = { rank= Seven;    suit= Spades }
let S8  = { rank= Eight;    suit= Spades }
let S9  = { rank= Nine;     suit= Spades }
let S10 = { rank= Ten;      suit= Spades }
let SJ  = { rank= Jack;     suit= Spades }
let SQ  = { rank= Queen;    suit= Spades }
let SK  = { rank= King;     suit= Spades }

let DA =  { rank= Ace;      suit= Diamonds }
let D2  = { rank= Two;      suit= Diamonds }
let D3  = { rank= Three;    suit= Diamonds }
let D4  = { rank= Four;     suit= Diamonds }
let D5  = { rank= Five;     suit= Diamonds }
let D6  = { rank= Six;      suit= Diamonds }
let D7  = { rank= Seven;    suit= Diamonds }
let D8  = { rank= Eight;    suit= Diamonds }
let D9  = { rank= Nine;     suit= Diamonds }
let D10 = { rank= Ten;      suit= Diamonds }
let DJ  = { rank= Jack;     suit= Diamonds }
let DQ  = { rank= Queen;    suit= Diamonds }
let DK  = { rank= King;     suit= Diamonds }

let HA =  { rank= Ace;      suit= Hearts }
let H2  = { rank= Two;      suit= Hearts }
let H3  = { rank= Three;    suit= Hearts }
let H4  = { rank= Four;     suit= Hearts }
let H5  = { rank= Five;     suit= Hearts }
let H6  = { rank= Six;      suit= Hearts }
let H7  = { rank= Seven;    suit= Hearts }
let H8  = { rank= Eight;    suit= Hearts }
let H9  = { rank= Nine;     suit= Hearts }
let H10 = { rank= Ten;      suit= Hearts }
let HJ  = { rank= Jack;     suit= Hearts }
let HQ  = { rank= Queen;    suit= Hearts }
let HK  = { rank= King;     suit= Hearts }

let CA =  { rank= Ace;      suit= Clubs }
let C2  = { rank= Two;      suit= Clubs }
let C3  = { rank= Three;    suit= Clubs }
let C4  = { rank= Four;     suit= Clubs }
let C5  = { rank= Five;     suit= Clubs }
let C6  = { rank= Six;      suit= Clubs }
let C7  = { rank= Seven;    suit= Clubs }
let C8  = { rank= Eight;    suit= Clubs }
let C9  = { rank= Nine;     suit= Clubs }
let C10 = { rank= Ten;      suit= Clubs }
let CJ  = { rank= Jack;     suit= Clubs }
let CQ  = { rank= Queen;    suit= Clubs }
let CK  = { rank= King;     suit= Clubs }

let Gin01  = seq [S5;S7;S8;S3;S9;S10;SA;S4;S6;S2]
let Gin02  = seq [H3;D3;C7;CA;H7;D7;SA;DA;S3;HA]
let Gin03  = seq [D4;H3;H4;S5;H5;H6;S3;D3;D5;S4]
let Gin04 = seq [S7;H3;D3;C7;CA;H7;D7;SA;DA;S3;HA]
let Hand01 = seq [CA;HA;H2;S3;H3;H4;SA;S4;C4;S2]
let Hand02 = seq [S2;H3;D3;C7;CA;H7;D7;SA;DA;S3;HA]
let Hand06 = seq [H3;H5;C3;S3;S4;S5;SA;D3;H4;S2]
let Hand10 = seq [S5;S7;S8;S3;S9;C10;SA;S4;S6;S2]
let Hand14 = seq [C3;C5;D5;S3;H5;C7;SA;H3;C4;S2]
let Hand18 = seq [C9;S5;S7;S8;S3;S9;C8;SA;S4;S6;S2]
let Hand19 = seq [S5;S7;S8;S3;C9;C10;SA;S4;S6;S2]
let Hand20 = seq [H10;C6;C8;D8;H4;C10;D10;SA;D5;C7;S2]
let Hand21 = seq [S3;C4;C5;SA;S9;S10;HA;S2;C3;DA]

let Hand22 = seq [C3;D2;DA;S3;S9;S10;SA;H3;D3;S2]

let Hand27 = seq [S5;S7;C8;S3;S9;S10;SA;S4;S6;S2]
let Hand29 = seq [S3;H3;C8;DA;S9;S10;SA;S2;C3;HA]
let Hand30 = seq [C3;C5;C8;S3;S9;S10;SA;H3;C4;S2]
let Hand32 = seq [S3;S7;H7;CA;S9;H9;SA;H3;D3;HA]
let Hand33 = seq [S5;C7;C8;S3;S9;H9;SA;S4;S6;S2]
let Hand34 = seq [S5;C7;C8;S3;S9;SJ;SA;H5;D5;S2]
let Hand36 = seq [C6;H3;H6;S7;D3;D8;S2;C5;C8;S6]
let Hand37 = seq [H10;D5;C7;C8;H3;C10;D10;SA;H4;D6;S3]
let Hand40 = seq [S5;C7;C8;S3;S9;S10;SA;S4;D6;S2]
let Hand42 = seq [S5;S7;H7;CA;S9;H9;SA;DA;H5;HA]
let Hand43 = seq [C3;C7;C8;S3;S10;SJ;SA;H3;D5;S2]
let Hand45 = seq [D5;C7;C8;S3;S9;S10;SA;S4;D6;S2]
let Hand46 = seq [S5;S7;H7;CA;S9;HJ;SA;H3;H5;HA]
let Hand49 = seq [D5;C7;C8;S3;S9;S10;SA;H4;D6;S2]
let Hand50 = seq [S5;S7;H7;S3;S9;H9;SA;H3;H5;HA]
let Hand55 = seq [D5;C7;C8;H3;S9;S10;SA;H4;D6;S2]
let Hand64 = seq [H10;D5;C7;C8;H3;S9;H9;SA;H4;D6;S2]
let Hand98 = seq [HJ;DJ;DK;C10;S9;C9;S10;CQ;HQ;SK]

let Deck01 = seq [D2;SJ;HJ;D5;D7;H10;CQ;H8;CA;H2;CJ;S8;S4;C2;H9;SA]
let Deck02 = seq [C4;H6;C6;DA;S4;D6;H4;S6;S5;D4;D2;C3;S2;S3;H2;D5;H5;HA;CA;C5;C2;H3;SA]

//let testCardAndHandWithSet = { card = queenHeart; hand =  testHand ; set = seq[seq[tenDiamond;tenClub;tenHeart]] ; run = seq [seq[eightHeart; nineHeart; tenHeart]; seq[spadeTwo;spadeThree;spadeFour]] }

    // Fixme: change so that it returns a shuffled deck


// Add other functions here related to Card Games ...