module GinRummy

open Cards

//let testCardAndHandWithSet = { card = queenHeart; hand =  testHand ; set = seq[seq[tenDiamond;tenClub;tenHeart]] ; run = seq [seq[eightHeart; nineHeart; tenHeart]; seq[spadeTwo;spadeThree;spadeFour]] }

let AllRanks = [ Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King ]

// Assigns a deadwood value for every card 
let cardValue (card:Card) =
    match card.rank with
    | Ace -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | _ -> 10

let sortBySuit (hand:Hand) =
    (Seq.sortBy (fun o -> AllRanks |> Seq.findIndex ((=) (o.rank))) hand)


let groupAndSortSuit (suit) (hand:Hand) =
     (Seq.filter (fun elem -> elem.suit = suit) hand) |> sortBySuit 

let listOfSortedSuits (hand : Hand) = 
    groupAndSortSuit Spades hand ::
    groupAndSortSuit Hearts hand ::
    groupAndSortSuit Diamonds hand ::
    groupAndSortSuit Clubs hand :: [] |> Seq.collect id

   
let testHand = seq [ eightHeart ;eightSpade; nineSpade  ; nineHeart ; tenHeart ; threeHeart ; heartTwo ; heartFour ; fiveHeart ; sixHeart; sevenHeart ; queenHeart ; tenDiamond ; tenClub ; tenSpade ; spadeTwo ; spadeThree ; spadeFour ; clubTwo ]
let testHandWithJustQueenAndKing = seq [jackHeart ; queenHeart ; heartKing]


let emptyHand = seq<Card>[]

let testCardAndHand = { card = Seq.head (listOfSortedSuits testHand); hand =  listOfSortedSuits testHand ; set = emptySeqOfCards ; run = emptySeqOfCards }

let testCardAndEmptyHandWithSetAndRun = { card = queenHeart; hand =  testHand ; set = seq[seq[tenDiamond;tenClub;tenHeart]] ; run = seq [seq[eightHeart; nineHeart; tenHeart]; seq[spadeTwo;spadeThree;spadeFour]] }
let testCardAndHandWithQueenAndKing = { card = jackHeart; hand = testHandWithJustQueenAndKing ; set = emptySeqOfCards ; run = emptySeqOfCards  }


let testCardAndEmptyHand = { card = queenHeart; hand =  emptyHand ; set = emptySeqOfCards  ; run = emptySeqOfCards}

let testCardAndHandSingleCard = { card = tenDiamond; hand = seq[tenDiamond] ; set = emptySeqOfCards  ; run = emptySeqOfCards}


let findIndexOfHand (card: Card, hand : Hand) =
    (Seq.findIndex (fun elem -> elem = card) hand) 
let sortByRank (hand:Hand) =
    (Seq.sortBy (fun o -> AllRanks |> Seq.findIndex ((=) (o.rank))) hand)

let getLowestValuedCard (hand:Hand) =
   hand |> sortByRank |> Seq.head

let findIndexOfRank (card:Card) =
    Seq.findIndex (fun elem -> elem = card.rank) cardRankSeq

let findNextRank (card:Card) =
    if (card.rank = King) then
       None
    else Some ( Seq.nth ((findIndexOfRank card) + 1) cardRankSeq)

// Given a card and a hand returns whether the next card in the hand is
let findNextCardInHand (card:Card, hand : Hand) =
    if ((findIndexOfHand( card, hand)) + 1 >= (Seq.length hand)) then 
        None
    else Some (Seq.nth ((Seq.findIndex (fun elem -> elem = card) hand) + 1 ) hand)
    
let tail (seq) =
    List.toSeq (List.tail (Seq.toList seq))
 


//Finds whether there exists a card which is an element of one sequence
let twoSeqHaveOneElementInCommon (seq1 : seq<Card> , seq2 : seq<Card>) =
    seq1 |> (fun seq1Element -> Seq.exists ((=)seq1Element)) 
 
// Finds whether or not there is an intersection between two sequences of cards
let rec findIntersectionBetweentwoSequences(seq1:seq<Card>, seq2:seq<Card>) =
    if (Seq.length seq1 >= 1) then
        let seq1Head = Seq.head seq1
        let seq1Tail = tail seq1
        if(seq2 |> Seq.exists (fun elem -> ((=) elem) seq1Head)) then 
            true
        else
            findIntersectionBetweentwoSequences (seq1Tail, seq2)
    else
        false

// Takes two sequences of sequences and an empty sequence of tuples and returns all the nested sequences that match the sequence 
let rec findIntersectionBetweenTwoSequencesOfSequences (seq1:seq<seq<Card>>, seq2:seq<seq<Card>>, seqOfTuples:seq<seq<Card>*seq<Card>>) =
    if (Seq.length seq1 > 0) then
        let seq1Head = Seq.head seq1
        let seq1Tail = tail seq1
        let newSeqOfTuples = seq2 |> Seq.map(fun seq2Mapped -> if (findIntersectionBetweentwoSequences(seq1Head,seq2Mapped)) then (seq1Head,seq2Mapped) else (seq<Card>[],seq<Card>[])) |> Seq.append seqOfTuples
        findIntersectionBetweenTwoSequencesOfSequences (seq1Tail, seq2, newSeqOfTuples)
    else
        seqOfTuples

// Given a sequence of cards, finds the accumalative deadwood value 
let getValueOfDeadWoodInSeq (cardSeq : seq<Card>) =
     cardSeq |> Seq.sumBy ( cardValue )


// finds the value of deadwood inside a tuple of card sequences
let findDeadWoodValueOfTuple (runAndSetTuple:seq<Card>*seq<Card>) = 
    let run,set = runAndSetTuple 
    (getValueOfDeadWoodInSeq run, getValueOfDeadWoodInSeq set )

// Takes a tuple of runs and sets and returns the one with the greater value, if they are equal it just returns the run
let chooseTheLargerValueInCardTuple (runAndSetTuple:seq<Card>*seq<Card>)=
    let run,set = runAndSetTuple 
    let runValue = getValueOfDeadWoodInSeq run
    let setValue = getValueOfDeadWoodInSeq set
    if (runValue > setValue) then
        run
    elif(setValue > runValue) then
        set
    else 
        run

// From the sequence of tuples of combined runs and sets, it is found whether the set containing a card and a run containing same card has a greater value. The higher value DW card is kept
let rec getMostValuableSeqs (seqOfTuples:seq<seq<Card>*seq<Card>>, bestValuedSeqs:seq<seq<Card>>) = 
    if (Seq.length seqOfTuples >= 1 ) then
        let seqOfTuplesTail = tail seqOfTuples
        let seqOfTuplesHead = Seq.head seqOfTuples
        let newBestValued = seqOfTuples |> Seq.map (fun setAndRun ->  chooseTheLargerValueInCardTuple setAndRun ) |> Seq.append bestValuedSeqs |> Seq.filter (fun elem -> not (Seq.length elem = 0)) |> Seq.distinctBy id
        getMostValuableSeqs (seqOfTuplesTail, newBestValued)
    else 
        bestValuedSeqs

let flterOutAllRunsAndSetsOfLeastValuableSeq (cardAndHand , bestValuedSeqs) =
    cardAndHand.run |> Seq.filter(fun runSeq -> findIntersectionBetweentwoSequences( runSeq, bestValuedSeqs  ))



// Check if the card is contained in the hand and is not identical 
let containsCard (cardAndHand : CardAndHand) = 
    Seq.exists (fun x -> cardAndHand.card = x ) cardAndHand.hand

// Finds the value of a set or run in terms of contributing deadwood
let valueSetOrRun (setOrRun:Card seq) =
    setOrRun |> Seq.sumBy ( cardValue )

// Increments the rank of the current card, if the card is a King than it just returns itself
let incrementRank (card : Card ) = 
    let rankOfNextCard = findNextRank card
    if (rankOfNextCard.IsNone) then 
        printfn "using king, handle this now"
        card
    else
        {suit = card.suit ; rank = Option.get<Rank>(rankOfNextCard) }

// Returns whether the next card is the same rank as the current card
let nextCardIsSameRank (cardAndHand : CardAndHand) =
    let rankOfCurrentCard = cardAndHand.card.rank
    let nextCardOption = findNextCardInHand (cardAndHand.card,cardAndHand.hand)
  //  let nextCard = Option.get<Card>( findNextCardInHand card hand)
    if ( nextCardOption.IsNone ) then
        false
        elif ((Option.get<Card>(nextCardOption)).rank = rankOfCurrentCard) then
            true
         else 
            false

// Returns whether the next card's rank is one up and is the same suit
let nextCardIsOneUpAndIsSameSuit (cardAndHand : CardAndHand) =
    let suitOfCurrentCard = cardAndHand.card.suit
    let currentCardsRank = cardAndHand.card.rank
    let nextCardsRankOption = findNextRank cardAndHand.card
    let nextCardOption = findNextCardInHand (cardAndHand.card ,cardAndHand.hand )
    if (nextCardOption.IsNone || nextCardsRankOption.IsNone) then
        false
        elif ((Option.get<Card>(nextCardOption)).rank = Option.get<Rank>(findNextRank cardAndHand.card) && (Option.get<Card>(nextCardOption)).suit = cardAndHand.card.suit) then
            true
         else 
            false

            (*
// Finds whether there is a run from the card in cardInHand and than traverses the hand until there is no run or it reaches the end of the hand
let rec nestedRunBuilder (cardAndHand : CardAndHand , run : seq<Card>) =
    let nextCard = findNextCardInHand (cardAndHand.card,cardAndHand.hand)
    let newRun  = Seq.append (seq [cardAndHand.card]) run
    if(nextCardIsOneUpAndIsSameSuit cardAndHand && not(nextCard.IsNone)) then
       printfn "%A" (Option.get<Card>( nextCard ) )
       nestedRunBuilder({card = Option.get<Card>( nextCard ) ; hand = tail cardAndHand.hand;  run = cardAndHand.run; set = cardAndHand.set } , newRun )      
     elif (nextCard.IsNone || ((Seq.length newRun > 3) && not(nextCardIsOneUpAndIsSameSuit cardAndHand))) then
        (Option.get<Card>(nextCard), newRun)
    else
        printfn "%A card %A run" nextCard run
        (Option.get<Card>(nextCard) , seq<Card>[])
        *)

let rec nestedRunBuilder (cardAndHand : CardAndHand , run : seq<Card>) =
     let nextCard = findNextCardInHand (cardAndHand.card,cardAndHand.hand)
     let newRun  = Seq.append (seq [cardAndHand.card]) run
    // printfn "Current card %A next card %A  next card is one up and is same suit %A" cardAndHand.card (nextCard) (nextCardIsOneUpAndIsSameSuit cardAndHand)
     if(nextCardIsOneUpAndIsSameSuit cardAndHand && not(nextCard.IsNone)) then
        nestedRunBuilder({card = Option.get<Card>( nextCard ) ; hand = tail cardAndHand.hand;  run = cardAndHand.run; set = cardAndHand.set } , newRun )      
      elif (nextCard.IsNone) then
         (cardAndHand.card, newRun)
      else
          (Option.get<Card>(nextCard) , Seq.append (seq [cardAndHand.card]) run)

// uses the nested run builder to find all the runs inside of the hand
let rec runBuilder (cardAndHand : CardAndHand ) =
    if(Seq.length cardAndHand.hand >= 1) then
        let card,run = nestedRunBuilder (cardAndHand , seq<Card>[])
        let newCardRuns = Seq.append (seq [run]) cardAndHand.run
        //printfn "%A %A" card run
        if ((findNextCardInHand (card,cardAndHand.hand) ).IsNone ) then
            if (Seq.length run >= 3) then
                newCardRuns
            else 
                cardAndHand.run
        elif(Seq.length run < 3 || not(nextCardIsOneUpAndIsSameSuit cardAndHand )) then
            runBuilder ({card = card ; hand = tail cardAndHand.hand; run = cardAndHand.run; set = cardAndHand.set })
        else
            runBuilder ({card = card ; hand = tail cardAndHand.hand; run = newCardRuns; set = cardAndHand.set })
    else
        cardAndHand.run



// finds all of the sets in the hand and returns them as a sequence of sequences
let rec populateSets (cardAndHand : CardAndHand) =
    if (Seq.length cardAndHand.hand >= 1) then
        let cardRank = cardAndHand.card.rank
        let currentSet = cardAndHand.hand |> Seq.filter (fun card -> card.rank = cardRank ) 
        if (Seq.length currentSet >= 3 ) then 
            populateSets ({card = Seq.head cardAndHand.hand ; hand = cardAndHand.hand |> Seq.filter (fun card -> not(card.rank = cardAndHand.card.rank)); set = Seq.append (seq[currentSet]) cardAndHand.set ; run = cardAndHand.run} )
        else 
            populateSets ({card = Seq.head cardAndHand.hand ; hand = cardAndHand.hand |> Seq.filter (fun card -> not(card.rank = cardAndHand.card.rank)) ; set = cardAndHand.set ; run = cardAndHand.run} )
    else
        cardAndHand.set |> Seq.filter (fun cardSeq -> not((Seq.length cardSeq) = 0))

//  Puts all of the sets and runs into the Card and Hand argument
let populateSetsAndRuns (cardAndHand: CardAndHand) =
    {card = cardAndHand.card ; hand = cardAndHand.hand ; set = cardAndHand |> populateSets; run = cardAndHand |> runBuilder}


   // Combines all the deadwood calculating functions and returns ...
let putDeadWoodCalculationTogether (cardAndHand: CardAndHand) =
    let IntersectionBetweenTwoSequencesOfSequences =  findIntersectionBetweenTwoSequencesOfSequences (cardAndHand.set, cardAndHand.run, seq<seq<Card>*seq<Card>>[]) 
    getMostValuableSeqs(IntersectionBetweenTwoSequencesOfSequences,  seq<seq<Card>>[]) 

// combines the set and run functions with the deadwood calculation
let combinedPopulatedSetsAndRunsWithDeadWoodCalculation (cardAndHand) =
    cardAndHand |> populateSetsAndRuns |> putDeadWoodCalculationTogether |> Seq.concat

    // Takes the 
let findLeftoverInsideHand (cardAndHand : CardAndHand) =
    cardAndHand.hand |> Seq.filter (fun elem ->(Seq.exists (fun card -> card = elem) (combinedPopulatedSetsAndRunsWithDeadWoodCalculation cardAndHand))=false ) 
    

// Constructs the card and hand state
let constructCardAndHand (hand: Hand) =
    let sortedHand = listOfSortedSuits hand
    {card = Seq.head sortedHand ; hand = sortedHand ; set = emptySeqOfCards ; run = emptySeqOfCards}


// Takes a hand and gets the value of the deadwood of the cards that for the remaining cards 
let Deadwood (hand:Hand) =
    let hand = listOfSortedSuits hand 
    //printfn "currentHand %A" hand
    let cardAndHand = constructCardAndHand hand |> populateSetsAndRuns
    //printfn "cardAndHand %A"cardAndHand
    let findLeftOver = findLeftoverInsideHand cardAndHand

    let sumOfLeftOver = Seq.sumBy(cardValue) findLeftOver

   // printfn "sum of left over%A" sumOfLeftOver
    sumOfLeftOver
   
   (* 
let Deadwood (hand:Hand) = 
    hand |> Seq.sumBy ( cardValue )
    *)
// Fixme change so that it computes the actual deadwood score

let Score (firstOut:Hand) (secondOut:Hand) =
    0
    // Fixme change so that it computes how many points should be scored by the firstOut hand
    // (score should be negative if the secondOut hand is the winner)

// dd other functions related to Gin Rummy here 
