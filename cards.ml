open Command

let cc_cards = [
  ("Advance to Enroll (Collect $200)", MoveTo 0);
  ("Bursar error in your favor - Collect $200", UpdateMoney 200);
  ("Cornell Health fees - Pay $50", UpdateMoney (-50));
  ("From sale of cryptocurrency you get $50", UpdateMoney 50);
  ("Get Out of JA's Office Free - This card will be kept until needed", AddCard true);
  ("Go directly to JA's Office - Do not pass Enroll - Do not collect $200", SendToJA);
  ("A Capella Concert - Collect $50 from every player for opening night seats", CollectFromAll 50);
  ("Grandma sends you holiday money - Receive $100", UpdateMoney 100);
  ("Tuition refund - Collect $20", UpdateMoney 20);
  ("It is your birthday - Collect $10 from each player", CollectFromAll 10);
  ("Bitcoin soars - Collect $100", UpdateMoney 100);
  ("Pay Cornell Health fees of $100", UpdateMoney (-100));
  ("Pay $150 for 'required' textbook you couldn't find online", UpdateMoney (-150));
  ("You find  $25 walking in Collegetown", UpdateMoney 25);
  ("Ithaca assesses you for street repairs - $40 per house - $115 per hotel", ForEachBuilding (40, 115));
  ("You have won second prize in an essay contest - Collect $10", UpdateMoney 10);
  ("You inherit $100", UpdateMoney 100);
]

let chance_cards = [
  ("Advance to Enroll (Collect $200)", MoveTo 0);
  ("Advance to Lincoln Hall - If you pass Enroll, collect $200", MoveTo 24);
  ("Advance to Martha Van Rensselaer Hall - If you pass Enroll, collect $200", MoveTo 11);
  ("Advance to Eduroam Wifi Network. If unowned, you may buy it from the Bank. If owned, pay rent", MoveTo 12);
  ("Advance to Red Rover Wifi Network. If unowned, you may buy it from the Bank. If owned, pay rent", MoveTo 28);
  ("Advance to Lyft. If unowned, you may buy it from the Bank. If owned, pay the owner rent", MoveTo 35);
  ("Bursar makes a mistake and pays you $50", UpdateMoney 50);
  ("Get Out of JA's Office Free - This card will be kept until needed", AddCard false);
  ("Go Back 3 Spaces", MoveForward (-3));
  ("Go directly to JA's Office - Do not pass Enroll - Do not collect $200", SendToJA);
  ("Make general repairs on all your properties - For each house pay $25 - For each hotel pay $100", ForEachBuilding (25, 100));
  ("Order takeout - Pay $15", UpdateMoney (-15));
  ("Take a trip on the TCAT Bus - If you pass Enroll, collect $200", MoveTo 5);
  ("Have a luxurious stay at the Statler Hotel - Advance to the Statler Hotel", MoveTo 39);
  ("You have been elected Chairman of the Board - Pay each player $50", CollectFromAll (-50));
  ("You were awarded a scholarship - Collect $150", UpdateMoney 150);
  ("You won a crossword competition - Collect $100", UpdateMoney 100);
]
