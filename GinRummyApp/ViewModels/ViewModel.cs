using Prism.Commands;
using Prism.Interactivity.InteractionRequest;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Threading.Tasks;
using System.Windows.Input;
using System.Linq;
using System.Collections.Generic;

namespace QUT
{
    class ViewModel: INotifyPropertyChanged
    {
        public ObservableCollection<Cards.Card> HumanCards { get; private set; }
        public ObservableCollection<Cards.Card> ComputerCards { get; private set; }
        public ObservableCollection<Cards.Card> Discards { get; private set; }
        public ObservableCollection<Cards.Card> RemainingDeck { get; private set; }



        public InteractionRequest<INotification> NotificationRequest { get; private set; }

        public ICommand ButtonCommand { get; set; }

        public ICommand GinCommand { get; set; }

        public ICommand ResetCommand { get; set; }
        public ICommand DiscardCardFromHandCommand { get; set; }
        public ICommand TakeCardFromDiscardPileCommand { get; set; }
        public ICommand TakeCardFromDeckCommand { get; set; }

        public event PropertyChangedEventHandler PropertyChanged;

        List<Cards.Card> computerSeen = new List<Cards.Card>();

        public ViewModel()
        {
// F sharp connection
            TakeCardFromDiscardPileCommand = new DelegateCommand<Cards.Card>(TakeCardFromDiscardPile);
            DiscardCardFromHandCommand = new DelegateCommand<Cards.Card>(DiscardCardFromHand);
            TakeCardFromDeckCommand = new DelegateCommand<Cards.Card>(TakeCardFromDeck);

            //Commands for the dedicated buttons
            GinCommand = new DelegateCommand(GinClick);
            ButtonCommand = new DelegateCommand(KnockClick);
            ResetCommand = new DelegateCommand(ResetClick);

            NotificationRequest = new InteractionRequest<INotification>();

            HumanCards = new ObservableCollection<Cards.Card>();
            ComputerCards = new ObservableCollection<Cards.Card>();
            Discards = new ObservableCollection<Cards.Card>();
            RemainingDeck = new ObservableCollection<Cards.Card>();


            

            var ComputerSeenDeck = Cards.FullDeck;
            foreach (var card in Cards.FullDeck)
            {
                computerSeen.Add(card);
            }


            HumanCards.CollectionChanged += HumanCards_CollectionChanged;

            Deal();
        }

        private async void Deal()
        {
            ComputerThoughts = "You dare challenge I? I can calculate more moves in a second than you can possibly imagine!";

            if (dealFinishied)
            {
                dealFinishied = false;
                var deck = Cards.Shuffle(Cards.FullDeck);

                foreach (var card in deck)
                {
                    RemainingDeck.Add(card);
                    await Task.Delay(1);
                }

                for (int i = 0; i < 10; i++)
                {
                    ComputerCards.Add(DrawTopCardFromDeck());
                    await Task.Delay(30);
                    HumanCards.Add(DrawTopCardFromDeck());
                    await Task.Delay(30);
                }

                Discards.Add(DrawTopCardFromDeck());
                dealFinishied = true;
            }
          

        }

        /*
         * 
        private async void ComputerDeck()
        {
            var deck = Cards.FullDeck;
            foreach (var card in deck)
            {
              

            }
        }
        */


        private Cards.Card DrawTopCardFromDeck()
        {
            var top = RemainingDeck[RemainingDeck.Count - 1];
            RemainingDeck.Remove(top);
            return top;
        }
        bool takenFromDeckOrDiscardPile = false;
        bool canNowDiscard = false;
        bool dealFinishied = true;


    // A global deadwood value, this is so that other functions can react accordingly 
    int Deadwood = 0;

        // Scores used to keep account of the current computer and humans scores
        int ComputerScore = 0;
        int HumanScore = 0;

        private void TakeCardFromDeck(Cards.Card card)
        {
            if (!takenFromDeckOrDiscardPile ) // Human goes first
            {
                RemainingDeck.Remove(card);
                HumanCards.Add(card);
                takenFromDeckOrDiscardPile = true;
                canNowDiscard = true;
            }
        }

        private void TakeCardFromDiscardPile(Cards.Card p)
        {
            if (!takenFromDeckOrDiscardPile)
            {
                Discards.Remove(p);
                HumanCards.Add(p);
                takenFromDeckOrDiscardPile = true;
                canNowDiscard = true;
            }
        }

        private void DiscardCardFromHand(Cards.Card p)
        {
            if (canNowDiscard)
            {
                HumanCards.Remove(p);
                Discards.Add(p);
                
                canNowDiscard = false;
                computerSeen.Remove(p);
                ArtificialPlayer();
            }
        } 

         async private void HumanCards_CollectionChanged(object sender, System.Collections.Specialized.NotifyCollectionChangedEventArgs e)
        {
            HumanDeadwood = "Calculating ...";
            // this might take a while, so let's do it in the background
            Deadwood = await Task.Run(() => GinRummy.Deadwood(HumanCards.ToArray()));
            
            //int deadwood = GinRummy.Deadwood(GinRummy.listOfSortedSuits(HumanCards).ToArray());
            HumanDeadwood = "Deadwood: " + Deadwood;
        }

        private string humanDeadwood;

        public string HumanDeadwood 
        { 
            get
            {
                return humanDeadwood;
            }
            private set
            {
                humanDeadwood = value;
                if (PropertyChanged != null)
                    PropertyChanged(this, new PropertyChangedEventArgs("HumanDeadwood"));
            }
        }


        private string computerThoughts;

        private string humanScoreString = "Score : 0";
        private string computerScoreString = "Score : 0" ;




        public string HumanScoreString
        {
            get
            {
                return humanScoreString;
            }
            private set
            {
                humanScoreString = value;
                if (PropertyChanged != null)
                    PropertyChanged(this, new PropertyChangedEventArgs("HumanScoreString"));
            }
        }

        public string ComputerScoreString
        {
            get
            {
                return computerScoreString;
            }
            private set
            {
                computerScoreString = value;
                if (PropertyChanged != null)
                    PropertyChanged(this, new PropertyChangedEventArgs("ComputerScoreString"));
            }
        }



        public string ComputerThoughts
        {
            get
            {
                return computerThoughts;
            }
            private set
            {
                computerThoughts = value;
                if (PropertyChanged != null)
                    PropertyChanged(this, new PropertyChangedEventArgs("ComputerThoughts"));
            }
        }

        private void RaiseNotification(string msg, string title)
        {
            NotificationRequest.Raise(new Notification { Content = msg, Title = title });
        }




        private void KnockClick()
        {
            if (Deadwood < 10 && dealFinishied)
            {
                int currentScore = GinRummy.Score(HumanCards, ComputerCards);
                int humanDeadwood = GinRummy.Deadwood(HumanCards);
                int computerDeadwood = GinRummy.Deadwood(ComputerCards);
                if (currentScore > 0)
                {
                    HumanScore += currentScore;
                    RaiseNotification("It seems I have underestimated you,  I only had a Deadwood of " + computerDeadwood + " while you had a "  + humanDeadwood, "Human Knock");
                }
                else {
                    ComputerScore += (currentScore * -1);
                    RaiseNotification("You fell into my trap human I had a score of " + ComputerScore + " while you had a weak score of " + HumanScore , "Human Undercut");
                }
                nextRound();
        // Do something to check what the current score is and see if it is the end of the game
        }

}

        private void ResetClick()
        {
            if (dealFinishied)
            {
                nextRound();
                HumanScoreString = "Score : 0";
                ComputerScoreString = "Score : 0";

                ComputerThoughts = "You dare challenge I? I can calculate more moves in a second than you can possibly imagine!";
            }
        }


        private void GinClick()
        {
            ComputerScore += 50;
            CheckForEndGame();

            if (Deadwood == 0 && dealFinishied)
            {
                int currentScore = GinRummy.Score(HumanCards, ComputerCards);
                if (currentScore > 0)
                {
                    HumanScore += currentScore;
                }
                // Do something to check what the current score is and see if it is the end of the game
            }
            else if (!dealFinishied)
            {
                RaiseNotification("Go back to 104, script kiddie.", "Can't go Gin during the deal");
            }
            else
            {
                RaiseNotification("You don't have a low enough Deadwood to go Gin yet, noob!", "Too much Deadwood!");
            }
        }

  

        async private void ArtificialPlayer()
        {
            if (takenFromDeckOrDiscardPile) 
            {
                while(true)
                {
                     ComputerThoughts = "Calculating your impending doom ...";


                    foreach (var card in ComputerCards)
                    {
                        computerSeen.Remove(card);
                    }

                    GinRummy.sortByRankAndSuit(ComputerCards);

                    var x = ComputerPlayer.averageAllOfThePossibleDeadwoods(computerSeen, ComputerCards);


                    var pickupFromDeckOrDiscard = await Task.Run(() => ComputerPlayer.ComputerPickupDiscard(ComputerCards, Discards.First(), computerSeen ));


                    if (pickupFromDeckOrDiscard) // picking up from the deck
                    {
                        ComputerThoughts = "You can not comprehend how much I know the top of the deck will benefit me, fool!";
                        ComputerCards.Add(DrawTopCardFromDeck());
                        await Task.Delay(5000);
                        var cardToAddToDiscard = ComputerPlayer.ComputerMove(ComputerCards);
                        ComputerCards.Remove(cardToAddToDiscard.Item2.Value);
                        Discards.Add(cardToAddToDiscard.Item2.Value);
                    } else { // picking up from the discard
                        ComputerThoughts = "Ahh this card will do very nicely, my thanks for allowing me to have it, fool!";

                        await Task.Delay(2000);

                        var topOfDiscard = Discards[Discards.Count - 1];
                        ComputerCards.Add(topOfDiscard);
                        Discards.Remove(topOfDiscard);
                        await Task.Delay(5000);
                        var cardToAddToDiscard = ComputerPlayer.ComputerMove(ComputerCards);
                        ComputerCards.Remove(cardToAddToDiscard.Item2.Value);
                        Discards.Add(cardToAddToDiscard.Item2.Value);
                    }
                    ComputerThoughts = "I now end my turn. Your move, human." + GinRummy.Deadwood(ComputerCards);

                    break;
                }
                canNowDiscard = false;
                takenFromDeckOrDiscardPile = false;
            }

        }

        private void nextRound() {
            CheckForEndGame();
            takenFromDeckOrDiscardPile = false;
            canNowDiscard = false;
            HumanCards.Clear();
            ComputerCards.Clear();
            RemainingDeck.Clear();
            Discards.Clear();
            Deal();
        }

        private void CheckForEndGame()
        {
            if (HumanScore >= 100 )
            {
                RaiseNotification("Noooo, how could I have been defeated by such a pathetic creature! \n" + "Human End Score = " + HumanScore + "\n" + "Computer End Score = " + ComputerScore  , "Human Winner");
                
            } else if (ComputerScore >= 100) {
                RaiseNotification("Just as I had planned, bow to me pathetic human for I am greater than you in every way! \n" + "Human End Score = " + HumanScore + "\n" + "Computer End Score = " + ComputerScore, "Machine Winner");
            }
        }

    }
}
