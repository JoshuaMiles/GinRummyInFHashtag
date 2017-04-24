module ComputerPlayer

open Cards
open GinRummy

type Move = Gin | Knock | Continue

let averageAllOfThePossibleDeadwoods (deck:seq<Card>,currentHand:seq<Card>) =
    (deck |> Seq.map (fun card -> Deadwood (Seq.append (seq[card]) currentHand) ) |> Seq.sum ) / ((Seq.length deck))

    (* if we assume that we are equally likely to draw any of the possible remaining cards
then we can compute the Deadwood score that we will obtain at the end of our turn by taking
the average of the Deadwood scores for all of the possible top cards on the deck*)
let ComputerPickupDiscard computerHand topDiscard possibleDeck =
    printfn "Discard %A  average of all possibilities %A"  (Deadwood (Seq.append (seq[topDiscard]) computerHand )) (averageAllOfThePossibleDeadwoods (possibleDeck |> Seq.filter (fun x -> not(x = topDiscard)), computerHand)) 
    let topDiscardDeadwood = Deadwood (Seq.append (seq[topDiscard]) computerHand )
    let averagedDeckDeadwood = averageAllOfThePossibleDeadwoods (possibleDeck |> Seq.filter (fun x -> not(x = topDiscard)), computerHand) 
    if (topDiscardDeadwood < averagedDeckDeadwood) then 
         true
    else
        false

let ComputerMove newHand =
    let card = Seq.head newHand
    let currentDeadwood = Deadwood newHand
    if (currentDeadwood = 0) then
        (Gin, None)
    elif (currentDeadwood <= 10 ) then 
        (Knock, Some card)
    else 
        (Continue, Some card)
