module GinRummy

open Cards

//let testCardAndHandWithSet = { card = queenHeart; hand =  testHand ; set = seq[seq[tenDiamond;tenClub;tenHeart]] ; run = seq [seq[eightHeart; nineHeart; tenHeart]; seq[spadeTwo;spadeThree;spadeFour]] }

let AllRanks = [ Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King ]

let mutable t = 0 


let emptyHand = seq<Card>[]
(*
let testCardAndHand = { card = Seq.head (listOfSortedSuits testHand ); hand = listOfSortedSuits testHand ; set = emptySeqOfCards ; run = emptySeqOfCards }

let testCardAndHand14 = { card = Seq.head (listOfSortedSuits Hand14 ); hand = listOfSortedSuits Hand14 ; set = emptySeqOfCards ; run = emptySeqOfCards }

*)

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

let sortByRank (hand:Hand) =
    (Seq.sortBy (fun o -> AllRanks |> Seq.findIndex ((=) (o.rank))) hand)


let groupAndSortSuit (suit) (hand:Hand) =
     (Seq.filter (fun elem -> elem.suit = suit) hand) |> sortByRank 

let sortByRankAndSuit (hand : Hand) = 
    groupAndSortSuit Spades hand ::
    groupAndSortSuit Hearts hand ::
    groupAndSortSuit Diamonds hand ::
    groupAndSortSuit Clubs hand :: [] |> Seq.collect id

let findIndexOfHand (card: Card, hand : Hand) =
    (Seq.findIndex (fun elem -> elem = card) hand) 


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
 
let reverse (seq) =
    List.toSeq (List.rev (Seq.toList seq))
 


//Finds whether there exists a card which is an element of one sequence
let twoSeqHaveOneElementInCommon (seq1 : seq<Card> , seq2 : seq<Card>) =
    seq1 |> (fun seq1Element -> Seq.exists ((=)seq1Element)) 
 
// Finds whether or not there is an intersection between two sequences of cards
let findIntersectionBetweentwoSequences (seq1: seq<Card>, seq2:seq<Card>) =
    Set.intersect (Set.ofSeq seq1) (Set.ofSeq seq2)


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


let chooseTheLowerValueInCardTuple (runAndSetTuple:seq<Card>*seq<Card>)=
    let run,set = runAndSetTuple 
    let runValue = getValueOfDeadWoodInSeq run
    let setValue = getValueOfDeadWoodInSeq set
    if (runValue < setValue) then
        run
    elif(setValue < runValue) then
        set
    else 
        run



let resultingHandValue (cardSequence,hand) = 
     hand |> Seq.filter (fun elem ->(Seq.exists (fun card -> card = elem) cardSequence)=false ) |> Seq.sumBy cardValue
    


// From the sequence of tuples of combined runs and sets, it is found whether the set containing a card and a run containing same card has a greater value. The higher value DW card is kept
let rec getMostValuableSeqs (seqOfTuples:seq<seq<Card>*seq<Card>>,hand, bestValuedSeqs:seq<seq<Card>>) = 
    if ((Seq.length seqOfTuples) >= 1 ) then
        let seqOfTuplesTail = tail seqOfTuples
        let seqOfTuplesHead = Seq.head 
        let newBestValued = seqOfTuples |> Seq.map (fun setAndRun ->  chooseTheLargerValueInCardTuple( setAndRun )) |> Seq.append bestValuedSeqs |> Seq.filter (fun elem -> not (Seq.length elem = 0)) |> Seq.distinctBy id
        getMostValuableSeqs (seqOfTuplesTail,hand, newBestValued)
    else 
        bestValuedSeqs

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
          
let rec nestedRunBuilder (cardAndHand : CardAndHand , run : seq<Card>) =
     let nextCard = findNextCardInHand (cardAndHand.card,cardAndHand.hand)
     let newRun  = Seq.append (seq [cardAndHand.card]) run
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

        if ((findNextCardInHand (card,cardAndHand.hand) ).IsNone ) then
            if (Seq.length run >= 3) then
                Seq.append (seq [run]) cardAndHand.run
            else
                cardAndHand.run 
        elif(Seq.length run < 3 || not(nextCardIsOneUpAndIsSameSuit cardAndHand ) || (Seq.exists (fun elem -> elem = run) cardAndHand.run)  ) then
            runBuilder ({card = card ; hand = tail cardAndHand.hand; run = cardAndHand.run; set = cardAndHand.set })
        else
            runBuilder ({card = card ; hand = tail cardAndHand.hand; run = (Seq.append (seq [run]) cardAndHand.run ); set = cardAndHand.set })
    else 
        cardAndHand.run

// Recursivly travels through every run and, if the run is greater than length 3, takes the head of the run and adds the tail 
let rec takeHeadReturnTailUntilThree(run:seq<Card>, allOtherRunsInRuns:seq<seq<Card>>) =
    let sortedReversedRun = sortByRank run |> reverse
    if (Seq.length run >= 2) then
        takeHeadReturnTailUntilThree(tail run, Seq.append (seq[tail run]) allOtherRunsInRuns)
    else
        allOtherRunsInRuns

let getAllPossibleRuns (cardAndHand) =
    let addedPossibleRuns = cardAndHand |> runBuilder |>  Seq.map ( fun elem -> takeHeadReturnTailUntilThree(elem,  seq<seq<Card>>[])) |> Seq.concat
    let allFullRuns = cardAndHand |> runBuilder 
    Seq.append addedPossibleRuns allFullRuns |> Seq.distinct




(*
1) those cards that could be used in either a set or a run, allExclusiveCards
2) those cards that could be used in a set, but not in a run, allExclusiveSetCards
3) those cards that could be used in a run, but not in a set, allExclusiveRunCards
4) those cards that can not be used in a set or a run. findLeftoverInsideHand
Create a helper (recursive) function that takes these 4 sets of cards as parameters. If the first set of cards is empty, then the deadwood score can be computed relatively straight forwardly. If the first set of cards is not empty, then take one of those cards and try adding it to the set of cards that can only be used to form runs. Then try adding it to the set of cards that can only be used to form sets. Check which of these (recursive function calls) leads to the lowest deadwood score.
*)
//Cards that are only in a sets but not in a run

let allExclusiveSetCards (cardAndHand) =
    cardAndHand.set |> Seq.concat |> Seq.distinct |> Seq.filter (fun x -> not(Seq.exists (fun elem ->(elem = x)) (Seq.concat cardAndHand.run |> Seq.distinct)) ) 

let allExclusiveRunCards (cardAndHand) =
    cardAndHand.run |> Seq.concat |> Seq.distinct |> Seq.filter (fun x -> not(Seq.exists (fun elem ->(elem = x)) (Seq.concat cardAndHand.set |> Seq.distinct)) ) 

let allExclusiveCards (cardAndHand) =
    let allSets = cardAndHand.set |> Seq.concat 
    let allRuns = cardAndHand.run |> Seq.concat 
    findIntersectionBetweentwoSequences (allSets, allRuns)

let populateSeqOfTuplesWithIntersection (run, set) = 
    run |> Seq.map(fun runMapped -> if ((Seq.length (findIntersectionBetweentwoSequences(Seq.head set, runMapped))) > 0) then (Seq.head set,runMapped) else (seq<Card>[],seq<Card>[]))  // The if statement wouldn't work without having an else, filtered it out later







// Takes a set and finds all combinations of the set 
let allCombinationsOfSetSequence (setSeq) =
    setSeq |> Seq.map ((fun x ->  Seq.append (Seq.filter (fun y -> not(x = y)) setSeq) (seq<Card>[]))) 

// finds all of the sets in the hand and returns them as a sequence of sequences
let rec populateSets (cardAndHand : CardAndHand) =
    if (Seq.length cardAndHand.hand >= 1) then
        let cardRank = cardAndHand.card.rank
        let currentSet = cardAndHand.hand |> Seq.filter (fun card -> card.rank = cardRank ) 
        if(Seq.length currentSet >= 4) then
            let combinationsOfAllSetSequences = Seq.append (allCombinationsOfSetSequence currentSet) cardAndHand.set 
            populateSets ({card = Seq.head cardAndHand.hand ; hand = cardAndHand.hand |> Seq.filter (fun card -> not(card.rank = cardAndHand.card.rank)); set = Seq.append (seq[currentSet])combinationsOfAllSetSequences  ; run = cardAndHand.run} )
        elif (Seq.length currentSet >= 3 ) then 
            populateSets ({card = Seq.head cardAndHand.hand ; hand = cardAndHand.hand |> Seq.filter (fun card -> not(card.rank = cardAndHand.card.rank)); set = Seq.append (seq[currentSet]) cardAndHand.set ; run = cardAndHand.run} )
        else 
            populateSets ({card = Seq.head cardAndHand.hand ; hand = cardAndHand.hand |> Seq.filter (fun card -> not(card.rank = cardAndHand.card.rank)) ; set = cardAndHand.set ; run = cardAndHand.run} )
    else
        cardAndHand.set |> Seq.filter (fun cardSeq -> not((Seq.length cardSeq) = 0))

//  Puts all of the sets and runs into the Card and Hand argument
let populateSetsAndRuns (cardAndHand: CardAndHand) =
    {card = cardAndHand.card ; hand = cardAndHand.hand ; set = cardAndHand |> populateSets; run = cardAndHand |> getAllPossibleRuns  |> Seq.filter (fun elem ->Seq.length elem >= 3) }

    
// Takes two sequences of sequences and an empty sequence of tuples and returns all sequences that fit the criteria tupled together
let rec findIntersectionBetweenTwoSequencesOfSequences (cardAndHand, seqOfTuples:seq<seq<Card>*seq<Card>>) =
    if (Seq.length cardAndHand.set > 0) then
        let newSeqOfTuples = populateSeqOfTuplesWithIntersection(cardAndHand.run, cardAndHand.set) |> Seq.append seqOfTuples 
        findIntersectionBetweenTwoSequencesOfSequences ({card = cardAndHand.card ; hand = cardAndHand.hand ; set = (tail cardAndHand.set) ; run = cardAndHand.run}, newSeqOfTuples)
    else
        seqOfTuples |> Seq.filter (fun (elem1, elem2)  -> (not(Seq.length elem1 = 0) ||  not(Seq.length elem2 = 0))) // filtering out all non conflicted leftovers

//takes a sequence of sequences and an empty sequence of tuples and compares all sequences to with itself and populates the sequence of tuples with the cards that are inside itself
let rec bruteForceFindBestmatch (sequenceOfSequences, sequenceOfTuples:seq<seq<Card>*seq<Card>>) =
    if ((Seq.length sequenceOfSequences) > 0 ) then 
        let head = Seq.head sequenceOfSequences 
        let tail = tail sequenceOfSequences
        //Look through list again looking for sequences that match 
        
        let updatedSequence = tail |> Seq.map (fun tailMapped ->  if((Seq.length (findIntersectionBetweentwoSequences( head , tailMapped))) >  0) then (head,tailMapped) else (seq<Card>[],seq<Card>[]) ) |> Seq.append sequenceOfTuples |> Seq.filter (fun (x,y) -> not(Seq.isEmpty x) ) 
        
        bruteForceFindBestmatch (tail,  updatedSequence )
    else 
        sequenceOfTuples 

// Takes a single element and a list of list of elements and checks if the list of list of elements contains the single element 
let checkIfContains (headElement, tailOftheList) = 
   tailOftheList |> Seq.map (fun cardSeq -> (Seq.exists (fun elem -> elem = headElement) cardSeq) ) |> Seq.exists (fun elem -> elem = true)


    // Takes a populated card and hand runs and the sets and removes all the cards from the hand that are within the runs and sets
    // dont do this until you filter out all the most valuable the first time 
let findLeftoverInsideHand (cardAndHand : CardAndHand) =
    cardAndHand.hand |> Seq.filter (fun elem ->(Seq.exists (fun card -> card = elem) (Seq.append (Seq.concat cardAndHand.run) (Seq.concat cardAndHand.set)))=false ) 
   
let findLeftOver (hand:Hand , ignore) =
  //  for x in ignore do printfn "$$$$$$$ Ignore %A $$$$$$" x
    hand |> Seq.filter (fun elem ->(Seq.exists (fun card -> card = elem) ignore)=false ) 
   


  //  findIntersectionBetweenTwoSequencesOfSequences (cardAndHand,seq<seq<Card>* seq<Card>>[]) <-- do first
  
let rec nonConflictingBestValuedSequences(conflicts,hand) =
    // find conflict between sets and runs
    let mostValuableSequences = getMostValuableSeqs (conflicts,hand, seq<seq<Card>>[]) 
    let bruteForced = bruteForceFindBestmatch (mostValuableSequences , seq<seq<Card>*seq<Card>>[]) 
    if ( Seq.length bruteForced > 0) then
        nonConflictingBestValuedSequences (bruteForced,hand)
    else
       // printfn "%A" mostValuableSequences
        mostValuableSequences

let getConflict (cardAndHand) =
    nonConflictingBestValuedSequences (findIntersectionBetweenTwoSequencesOfSequences(cardAndHand, seq<seq<Card>*seq<Card>>[]),cardAndHand.hand)

// Remove the cards that should now be ignored
let filteredHand (hand, toBeFiltered) =
   // printfn "%A"  (findLeftOver(hand, Seq.concat toBeFiltered) |> sortByRankAndSuit)
    findLeftOver(hand, Seq.concat toBeFiltered) |> sortByRankAndSuit

let filteredCards (hand, toBeFiltered) =
   // printfn "%A"  (findLeftOver(hand, Seq.concat toBeFiltered) |> sortByRankAndSuit)
    findLeftOver(hand, toBeFiltered) |> sortByRankAndSuit

let letFilteredCardAndHandAndpopulated (hand , toBeFiltered) =
    {card = (Seq.head (filteredHand( hand, toBeFiltered ))) ; hand = filteredHand( hand, toBeFiltered ); set = emptySeqOfCards ; run = emptySeqOfCards} |> populateSetsAndRuns
 
let rec getAllFirstIgnore (cardAndHand, ignore) =
    let newIgnore =  Seq.append (getConflict cardAndHand ) ignore

    let newPopulatedCardAndHand = letFilteredCardAndHandAndpopulated(cardAndHand.hand,newIgnore)
    
    if (((Seq.length newIgnore) > (Seq.length ignore))) then // To start removing cards from the hand, there needs to be a difference between the previous ignore and the current ignore
        
        if (((Seq.length newPopulatedCardAndHand.run > 0) || (Seq.length  newPopulatedCardAndHand.set) > 0) ) then

         //   printfn "inner if " 
            getAllFirstIgnore (newPopulatedCardAndHand, newIgnore )
        else
            //printfn "newPopulatedCardAndHand.hand   %A " newPopulatedCardAndHand.hand
            filteredHand(cardAndHand.hand,newIgnore )
    else 
         filteredHand(cardAndHand.hand, (Seq.append (cardAndHand.run) (cardAndHand.set)) )


    
let isIntersection(seq1: seq<Card>, seq2:seq<Card>) =
    (Set.count (Set.intersect (Set.ofSeq seq1) (Set.ofSeq seq2)) > 0) 
 


let allSetsAndRunsWithoutConflict (set,run) =
    run |> Seq.map (fun runSeq -> set |> Seq.map (fun setSeq ->  if (not(isIntersection(runSeq, setSeq))) then (Seq.append runSeq setSeq) else seq<Card>[])) |> Seq.map( Seq.filter (fun x -> not(Seq.length x = 0))) |> Seq.concat



let testFunction (cardAndHand) =
    let getAllIgnoreCardWithConflict = getAllFirstIgnore(cardAndHand, seq<seq<Card>>[])
    let getAllIgnoreCardWithOutConflict = allSetsAndRunsWithoutConflict(cardAndHand.run,cardAndHand.set) |> Seq.map (fun x -> filteredCards(cardAndHand.hand, x)) 
    getAllIgnoreCardWithOutConflict |>  Seq.map (fun noConflict -> (getAllIgnoreCardWithConflict,noConflict) )  //Seq.concat

  
let rec reduceToTheLowestValue(sequenceOfCards:seq<seq<Card>>, currentLowest) =
    printfn "%A" (Seq.length sequenceOfCards)
    if ((Seq.length sequenceOfCards)> 0 ) then 
        let sequenceHeadValue = Seq.head sequenceOfCards |> Seq.sumBy cardValue
        
        if (sequenceHeadValue > (currentLowest |> (Seq.sumBy cardValue))) then
            reduceToTheLowestValue(tail sequenceOfCards, Seq.head sequenceOfCards)
        else 
            reduceToTheLowestValue(tail sequenceOfCards, currentLowest)
    else
        printfn "%A" currentLowest
        currentLowest

let withOrWithoutConflict (cardAndHand)  =
    if((Seq.length (testFunction(cardAndHand))) > 0 ) then
        printfn "%A" (cardAndHand |> testFunction |> Seq.map(fun (tuple1,tuple2) -> if ((tuple1 |> Seq.sumBy cardValue) < (tuple2 |> Seq.sumBy cardValue)) then tuple1 else tuple2 ))
        let x = (cardAndHand |> testFunction |> Seq.map(fun (tuple1,tuple2) -> if ((tuple1 |> Seq.sumBy cardValue) <= (tuple2 |> Seq.sumBy cardValue)) then tuple1 else tuple2 )) 
        reduceToTheLowestValue(x, seq<Card>[])
    else 
        getAllFirstIgnore(cardAndHand, seq<seq<Card>>[])

let reduceToTheBestAtThisPointInTime(cardAndHand) =
    let getCurrentBest = withOrWithoutConflict (cardAndHand)
    match (Seq.length getCurrentBest) with 
    | 0 -> 0
    | _ -> getCurrentBest |> Seq.distinct |> Seq.sumBy cardValue

// Constructs the card and hand state
let constructCardAndHand (hand: Hand) =
    let sortedHand = sortByRankAndSuit hand
    {card = Seq.head sortedHand ; hand = sortedHand ; set = emptySeqOfCards ; run = emptySeqOfCards}

// Takes a hand and gets the value of the deadwood of the cards that for the remaining cards 
let Deadwood (hand:Hand) =
    //printfn "currentHand %A" 
      //  getAllUnecessary(populateSetsAndRuns  (constructCardAndHand (hand )), seq<seq<Card>>[])  
    reduceToTheBestAtThisPointInTime(populateSetsAndRuns (constructCardAndHand (hand)))
    
   // printfn "sum of left over%A" sumOfLeftOver


let Score (firstOut:Hand) (secondOut:Hand) =
    let firstHandScore = Deadwood firstOut
    let secondHandScore = Deadwood secondOut
    if (firstHandScore = 0 ) then 
        secondHandScore + 25
     elif (firstHandScore < secondHandScore) then
        secondHandScore - firstHandScore
    else 
        (-firstHandScore) - 25

    // Fixme change so that it computes how many points should be scored by the firstOut hand
    // (score should be negative if the secondOut hand is the winner)

// dd other functions related to Gin Rummy here 