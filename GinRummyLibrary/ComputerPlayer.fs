module ComputerPlayer

open Cards
open GinRummy

type Move = Gin | Knock | Continue



let averageAllOfThePossibleDeadwoods (deck:seq<Card>,currentHand:seq<Card>) =
    let deckLength = Seq.length deck
    let sumOfDeadwoodDeck =  deck |> Seq.map (fun card -> Deadwood (Seq.append (seq[card]) currentHand) ) |> Seq.sum 
    sumOfDeadwoodDeck/deckLength

    (* if we assume that we are equally likely to draw any of the possible remaining cards
then we can compute the Deadwood score that we will obtain at the end of our turn by taking
the average of the Deadwood scores for all of the possible top cards on the deck*)
let ComputerPickupDiscard(computerHand:Hand, topDiscard:Card, possibleDeck:seq<Card>) =
    
    let topDiscardDeadwood = Deadwood (Seq.append (seq[topDiscard]) computerHand )

    let averagedDeckDeadwood = averageAllOfThePossibleDeadwoods (possibleDeck |> Seq.filter (fun x -> not(x = topDiscard)), computerHand) 
    if (topDiscardDeadwood < averagedDeckDeadwood) then 
         true
    else
        false
    // Fixme: change function so that it computes if Computer should pickup from Discard pile 
    //        or draw a fresh card from the deck

let ComputerMove newHand =
    let card = Seq.head newHand
    let currentDeadwood = Deadwood newHand
    if (currentDeadwood = 0) then
        (Gin, None)
    elif (currentDeadwood < 10 ) then 
        (Knock, Some card)
    else 
        (Continue, Some card)
    // Fixme: change function so that it computes which action the Computer should take: Continue, Knock or Gin 
    //        and which card would be best to discard

