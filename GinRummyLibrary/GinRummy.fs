module GinRummy

open Cards

//let testCardAndHandWithSet = { card = queenHeart; hand =  testHand ; set = seq[seq[tenDiamond;tenClub;tenHeart]] ; run = seq [seq[eightHeart; nineHeart; tenHeart]; seq[spadeTwo;spadeThree;spadeFour]] }

let AllRanks = [ Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King ]

let testHand = seq [ eightHeart ;eightSpade; nineSpade  ; nineHeart ; tenHeart ; threeHeart ; heartTwo ; heartFour ; fiveHeart ; sixHeart; sevenHeart ; queenHeart ; tenDiamond ; tenClub ; tenSpade ; spadeTwo ; spadeThree ; spadeFour ; clubTwo ]
let testHandWithFullSet = seq [threeHeart ; diamondThree ; spadeThree ; clubThree]

let testHandFaulty = seq [diamondQueen;  diamondTen ; diamondEight ; diamondSix ;diamondFour;diamondTwo ; jackHeart ; nineHeart ;sevenHeart ;  sixHeart; fiveHeart  ]
let testCardAndHandFaulty = { card = fiveHeart; hand =  testHandFaulty ; set = emptySeqOfCards; run =emptySeqOfCards}

let testCardAndHandFullSet = { card = Seq.head ( testHandWithFullSet ); hand =  testHandWithFullSet ; set = emptySeqOfCards ; run = emptySeqOfCards }


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

let sortBySuit (hand:Hand) =
    (Seq.sortBy (fun o -> AllRanks |> Seq.findIndex ((=) (o.rank))) hand)


let groupAndSortSuit (suit) (hand:Hand) =
     (Seq.filter (fun elem -> elem.suit = suit) hand) |> sortBySuit 

let listOfSortedSuits (hand : Hand) = 
    groupAndSortSuit Spades hand ::
    groupAndSortSuit Hearts hand ::
    groupAndSortSuit Diamonds hand ::
    groupAndSortSuit Clubs hand :: [] |> Seq.collect id

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

// From the sequence of tuples of combined runs and sets, it is found whether the set containing a card and a run containing same card has a greater value. The higher value DW card is kept
let rec getMostValuableSeqs (seqOfTuples:seq<seq<Card>*seq<Card>>, bestValuedSeqs:seq<seq<Card>>) = 
    if (Seq.length seqOfTuples >= 1 ) then
        let seqOfTuplesTail = tail seqOfTuples
        let seqOfTuplesHead = Seq.head 
        let newBestValued = seqOfTuples |> Seq.map (fun setAndRun ->  chooseTheLargerValueInCardTuple setAndRun ) |> Seq.append bestValuedSeqs |> Seq.filter (fun elem -> not (Seq.length elem = 0)) |> Seq.distinctBy id
        getMostValuableSeqs (seqOfTuplesTail, newBestValued)
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
(*
let rec nestedRunBuilder (cardAndHand : CardAndHand , run : seq<Card>) =
     let nextCard = findNextCardInHand (cardAndHand.card,cardAndHand.hand)
     let newRun  = Seq.append (seq [cardAndHand.card]) run 
     if(nextCardIsOneUpAndIsSameSuit cardAndHand ) then
        for x in newRun do printfn " //////////// %A    //////////////"x
        nestedRunBuilder({card = Option.get<Card>( nextCard ) ; hand = tail cardAndHand.hand;  run = (Seq.append (seq[newRun]) cardAndHand.run) ; set = cardAndHand.set} , newRun )
       
      elif (nextCard.IsNone && Seq.length newRun >= 2) then
         (cardAndHand.card, (Seq.append (seq[newRun]) cardAndHand.run))// changed this because there is a duplicate
      elif (nextCard.IsNone) then
         (cardAndHand.card, (cardAndHand.run))
      else
          // let filteredCardAndRun = cardAndHand.run |>  Seq.filter (fun elem -> (Seq.length elem) >= 3 )

          printfn "next card %A" nextCard
          (Option.get<Card>(nextCard) , cardAndHand.run)
          //(Option.get<Card>(nextCard) , Seq.append (seq [cardAndHand.card]) run) // Returns the card finishing on and the run
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
        
        //printfn "%A %A" card run
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
    {card = cardAndHand.card ; hand = cardAndHand.hand ; set = cardAndHand |> populateSets; run = cardAndHand |> runBuilder |> Seq.distinctBy id}
    
// Takes two sequences of sequences and an empty sequence of tuples and returns all sequences that fit the criteria tupled together
let rec findIntersectionBetweenTwoSequencesOfSequences (cardAndHand, seqOfTuples:seq<seq<Card>*seq<Card>>) =
    let set = cardAndHand.set
    let run = cardAndHand.run
    if (Seq.length set > 0) then
        let setHead = Seq.head set
        let setTail = tail set
        let newSeqOfTuples = run |> Seq.map(fun runMapped -> if ((Seq.length (findIntersectionBetweentwoSequences(setHead, runMapped))) > 0) then (setHead,runMapped) else (seq<Card>[],seq<Card>[])) |> Seq.append seqOfTuples 
        findIntersectionBetweenTwoSequencesOfSequences ({card = cardAndHand.card ; hand = cardAndHand.hand ; set = setTail ; run = run}, newSeqOfTuples)
    else
        seqOfTuples |> Seq.filter (fun (elem1, elem2)  -> (not(Seq.length elem1 = 0) ||  not(Seq.length elem2 = 0))) // filtering out all non conflicted leftovers

//takes a sequence of sequences and an empty sequence of tuples and compares all sequences to with itself and populates the sequence of tuples with the cards that are inside itself
let rec bruteForceFindBestmatch (sequenceOfSequences, sequenceOfTuples:seq<seq<Card>*seq<Card>>) =
    if ((Seq.length sequenceOfSequences) > 0 ) then 
        let head = Seq.head sequenceOfSequences 
        let tail = tail sequenceOfSequences
        //Look through list again looking for sequences that match 
        let updatedSequence = tail |> Seq.map (fun tailMapped -> if((Seq.length (findIntersectionBetweentwoSequences( head , tailMapped))) >  0) then (head,tailMapped) else (seq<Card>[],seq<Card>[]) ) |> Seq.append sequenceOfTuples
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
    hand |> Seq.filter (fun elem ->(Seq.exists (fun card -> card = elem) ignore)=false ) 
   


  //  findIntersectionBetweenTwoSequencesOfSequences (cardAndHand,seq<seq<Card>* seq<Card>>[]) <-- do first

let rec nonConflictingBestValuedSequences(conflicts) =
    // find conflict between sets and runs
    let mostValuableSequences = getMostValuableSeqs (conflicts, seq<seq<Card>>[]) //conflicts = seq of tuples
    let bruteForced = bruteForceFindBestmatch (mostValuableSequences , seq<seq<Card>*seq<Card>>[]) 
    if (Seq.length bruteForced > 0) then
        nonConflictingBestValuedSequences (bruteForced)
    else
        mostValuableSequences
         
let rec getAllNecessary (cardAndHand, ignore) =
    let newIgnore =  Seq.append (nonConflictingBestValuedSequences (findIntersectionBetweenTwoSequencesOfSequences(cardAndHand, seq<seq<Card>*seq<Card>>[]) )) ignore
    let hand = cardAndHand.hand 
    //let removedCardsFromHand = cardAndHand.hand |> Seq.filter (fun elem ->(Seq.exists (fun card -> card = elem) (newIgnore)=false ) 
    let newHand=findLeftOver(cardAndHand.hand, Seq.concat newIgnore)
    let newPopulatedCardAndHand = {card = (Seq.head newHand) ; hand = newHand ; set = emptySeqOfCards ; run = emptySeqOfCards} |> populateSetsAndRuns

    if (((Seq.length newPopulatedCardAndHand.run > 0) || (Seq.length  newPopulatedCardAndHand.set) > 0) &&  (Seq.length newIgnore > 0 )  ) then
        getAllNecessary (newPopulatedCardAndHand, newIgnore)
    else
        newHand

    

// Constructs the card and hand state
let constructCardAndHand (hand: Hand) =
    let sortedHand = listOfSortedSuits hand
    {card = Seq.head sortedHand ; hand = sortedHand ; set = emptySeqOfCards ; run = emptySeqOfCards}


// Takes a hand and gets the value of the deadwood of the cards that for the remaining cards 
let Deadwood (hand:Hand) =
    //printfn "currentHand %A" hand
    let cardAndHand = constructCardAndHand (hand )
    
    getAllNecessary(cardAndHand, seq<seq<Card>>[]) |> Seq.sumBy(cardValue)
   
   // printfn "sum of left over%A" sumOfLeftOver


let Score (firstOut:Hand) (secondOut:Hand) =
    0
    // Fixme change so that it computes how many points should be scored by the firstOut hand
    // (score should be negative if the secondOut hand is the winner)

// dd other functions related to Gin Rummy here 
