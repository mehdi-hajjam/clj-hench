(ns hench.shout)

(def sfquotes
  [;Ken Masters from https://streetfighter.fandom.com/wiki/Ken_Masters/Quotes
   "True strength is something money and credit cards cannot buy!"
   "Strong fighters such as yourself make it worth staying in shape!"
   "Shoryureppa...! Shinryuken...! Feel my burning vigor!"
   "Rivalry can often inspire one's skill to become its best!"
   "In the heat of battle, the blood of the true fighter runs hot!"
   "I need a better workout than this! Where's Eliza?!"
   "Go back! I think you left your ego on the battlefield!"
   "Fights like this bring out the best in me!"
   "Have you woken up yet?"
   "This is the difference in ability!"
   "You can't win with attacks like that!"
   "I am the man!"
   "Damn! You wrinkled my clothes!"
   "Sorry, but I had to impress the ladies."
   "Next time, put some money on the line."
   "Looks, speed, and strength. I’ve got it all."
   ;Ryu from https://streetfighter.fandom.com/wiki/Ryu/Quotes
   "What strength!! But don't forget there are many guys like you all over the world."
   "I wish you good luck!"
   "You did quite well, but you need more training to defeat me!"
   "To live is to fight, to fight is to live!"
   "There's nothing like a fair fight. It improves both competitors."
   "You must love competition before you can achieve victory."
   "Don't fight for victory--fight to improve yourself. Victory will come."
   "This was a battle of spirits, not fists. Search your soul if you want to beat me!"
   "I really broke a sweat! How about another round?"
   "Blind pride can only hold you back. There are fighters out there stronger than the both of us!"
   "If you want to be a true fighter... go home and train!"
   "I know you can do better! Get up and try again!"
   ;Ken le survivant
   "Omae wa mo... shinde iru!"])

(defn random-shout
  [quotes]
  (rand-nth quotes))