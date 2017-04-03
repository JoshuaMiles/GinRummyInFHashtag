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

    // Fixme: change so that it returns a shuffled deck


// Add other functions here related to Card Games ...