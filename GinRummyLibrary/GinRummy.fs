module GinRummy


open Cards



let AllRanks = [ Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King ]

let heartTwo  = {suit = Hearts ; rank = Two }

let heartThree = {suit = Hearts ; rank = Three }

let heartFour = {suit = Hearts ; rank = Four }

let heartKing =  {suit = Hearts ; rank = King }

let spadeTwo  = {suit = Spades ; rank = Two }

let spadeThree = {suit = Spades ; rank = Three } 

let spadeFour = {suit = Spades ; rank = Four }

let spadeKing =  {suit = Spades ; rank = King }

let shuffledHand = seq [ spadeFour; spadeKing ; spadeThree ; heartKing  ; spadeTwo ; heartFour; heartThree ; heartTwo  ]

let matchRank (card:Card) =
    match card.suit with
        | Hearts -> Hearts
        | Spades -> Spades
        | Diamonds -> Diamonds
        | Clubs -> Clubs


let unorderedRunHand = seq [heartFour; heartThree; heartTwo ]

let orderedRunHand = seq [ heartTwo; heartThree; heartFour ]

let orderedRunHandWithAddedCard = seq [ heartTwo; heartThree; heartFour ; spadeTwo ]


let partialRunHand = seq [ heartTwo; heartThree ]
        

let cardValue (card:Card) =
    match card.rank with
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | _ -> 10


let cardRankSeq = seq [ Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King ]

type CardAndHand = {card : Card ; hand : Hand}

let cardAndHandExample = {card = spadeFour; hand = shuffledHand}



let findIndexOfHand (cardAndHand : CardAndHand) =
    (Seq.findIndex (fun elem -> elem = cardAndHand.card) cardAndHand.hand) 



let sortBySuit (hand:Hand) =
    (Seq.sortBy (fun o -> AllRanks |> Seq.findIndex ((=) (o.rank))) hand)

let groupAndSortSuit (suit) (hand:Hand) =
     (Seq.filter (fun elem -> elem.suit = suit) hand) |> sortBySuit 

let listOfSortedSuits (hand : Hand) = 
    groupAndSortSuit Spades hand ::
    groupAndSortSuit Hearts hand ::
    groupAndSortSuit Diamonds hand ::
    groupAndSortSuit Clubs hand :: [] |> Seq.collect id


let findIndexOfRank (card:Card) =
    Seq.findIndex (fun elem -> elem = card.rank) cardRankSeq


let findNextRank (card:Card) =
    if (card.rank = King) then
       None
    else Some ( Seq.nth ((findIndexOfRank card) + 1) cardRankSeq)

let findNextCardInHand (cardAndHand : CardAndHand) =
    if ((findIndexOfHand cardAndHand) + 1 >= (Seq.length cardAndHand.hand)) then 
        None
    else Some (Seq.nth ((Seq.findIndex (fun elem -> elem = cardAndHand.card) cardAndHand.hand) + 1 )  cardAndHand.hand)


let nextCardIsOneUpAndIsSameSuit (cardAndHand : CardAndHand) =
    let suitOfCurrentCard = cardAndHand.card.suit
    let currentCardsRank = cardAndHand.card.rank
    let nextCardsRankOption = findNextRank cardAndHand.card
    let nextCardOption = findNextCardInHand cardAndHand
  //  let nextCard = Option.get<Card>( findNextCardInHand card hand)
    if (nextCardOption.IsNone || nextCardsRankOption.IsNone) then
        false
        elif ((Option.get<Card>(nextCardOption)).rank = Option.get<Rank>(findNextRank cardAndHand.card) && (Option.get<Card>(nextCardOption)).suit = cardAndHand.card.suit) then
            true
         else 
            false

let cahUS = {card = spadeTwo; hand =  listOfSortedSuits shuffledHand}



let cahOrd = {card= heartTwo ; hand = orderedRunHandWithAddedCard}

let rec howManyInARow (cardAndHand: CardAndHand) =
    let nextCard = nextCardIsOneUpAndIsSameSuit cardAndHand
    if (nextCard) then
         1 + (howManyInARow {hand = cardAndHand.hand ; card = (Option.get<Card>(findNextCardInHand cardAndHand))})
    else
        0


let rec nestedRunBuilder (cardAndHand : CardAndHand , run : seq<Card>) =
    let nextCard = findNextCardInHand cardAndHand

    let newRun  =Seq.append (seq [cardAndHand.card]) run
    if(nextCardIsOneUpAndIsSameSuit cardAndHand) then
       nestedRunBuilder({card = Option.get<Card>( nextCard ) ; hand = Seq.tail cardAndHand.hand }, newRun )      
     else
        newRun


let rec runBuilder (cardAndHand : CardAndHand , cardRuns : seq<seq<Card>>) =
    let currentRun = nestedRunBuilder (cardAndHand , seq<Card>[])
    let newCardRuns = Seq.append (seq [currentRun]) cardRuns
    if (Seq.length cardAndHand.hand = 0 || (findNextCardInHand cardAndHand).IsNone) then
        newCardRuns
    else 
        runBuilder ({card = Option.get<Card> (findNextCardInHand cardAndHand) ; hand = Seq.tail cardAndHand.hand }, newCardRuns)

        
 // Takes an already ordered hand and the first element in the array and sums all the runs in the hand
let ScoreRunsInHand (cardAndHand : CardAndHand) = 
    runBuilder(cardAndHand, seq<seq<Card>>[]) |> Seq.filter (fun y -> Seq.length y >= 3) |> Seq.map(fun z ->  Seq.sumBy (cardValue) z) |> Seq.reduce (fun x y -> x + y)

let Deadwood (hand:Hand) = 
   hand |> Seq.sumBy ( cardValue )
    
    // Fixme change so that it computes the actual deadwood score

let Score (firstOut:Hand) (secondOut:Hand) =
    0
    // Fixme change so that it computes how many points should be scored by the firstOut hand
    // (score should be negative if the secondOut hand is the winner)

// dd other functions related to Gin Rummy here ...