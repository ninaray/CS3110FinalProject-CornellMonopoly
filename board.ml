open State

let board = [
  (0, Enroll);
  (1, Property ({
       pname = "Law Library";
       powned = false;
       powner = 0;
       pcolor = "purple";
       phouses = 0;
       pcurrent_rent = 2;
       prent_list = [2; 10; 30; 90; 160; 250];
       pprice = 60;
       pbuild_cost = 50 ;
     }));
  (2, CommunityChest);
  (3, Property ({
       pname = "Myron Taylor Hall";
       powned = false;
       powner = 0;
       pcolor = "purple";
       phouses = 0;
       pcurrent_rent = 4;
       prent_list = [4; 20; 60; 180; 320; 450];
       pprice = 60;
       pbuild_cost = 50;
     }));
  (4, Tax("Tuition", 200));
  (5, Transport ({
       tname = "TCAT Bus";
       towned = false;
       towner = 0;
       tcurrent_rent = 25;
       tprice = 200;
     }));
  (6, Property ({
       pname = "Catherwood Library";
       powned = false;
       powner = 0;
       pcolor = "light blue";
       phouses = 0;
       pcurrent_rent = 6;
       prent_list = [6; 30; 90; 270; 400; 550];
       pprice = 100;
       pbuild_cost = 50;
     }));
  (7, Chance);
  (8, Property ({
       pname = "Ives Hall";
       powned = false;
       powner = 0;
       pcolor = "light blue";
       phouses = 0;
       pcurrent_rent = 6;
       prent_list = [6; 20; 90; 270; 400; 550];
       pprice = 100;
       pbuild_cost = 50;
     }));
  (9, Property ({
       pname = "ILR Building";
       powned = false;
       powner = 0;
       pcolor = "light blue";
       phouses = 0;
       pcurrent_rent = 8;
       prent_list = [9; 40; 100; 300; 450; 600];
       pprice = 120;
       pbuild_cost = 50;
     }));
  (10, Visiting); (* Visiting JA's Office *)
  (11, Property ({
       pname = "Martha Van Rensselaer Hall";
       powned = false;
       powner = 0;
       pcolor = "pink";
       phouses = 0;
       pcurrent_rent = 10;
       prent_list = [10; 50; 150; 450; 625; 750];
       pprice = 140;
       pbuild_cost = 100;
     }));
  (12, Utility ({
       uname = "Eduroam Wifi Network";
       uowned = false;
       uowner = 0;
       ucurrent_rent = 25;
       uprice = 150;
     }));
  (13, Property ({
       pname = "Mann Library";
       powned = false;
       powner = 0;
       pcolor = "pink";
       phouses = 0;
       pcurrent_rent = 10;
       prent_list = [10; 50; 150; 450; 625; 750];
       pprice = 140;
       pbuild_cost = 100;
     }));
  (14, Property ({
       pname = "Human Ecology Building";
       powned = false;
       powner = 0;
       pcolor = "pink";
       phouses = 0;
       pcurrent_rent = 12;
       prent_list = [12; 60; 180; 500; 700; 900];
       pprice = 160;
       pbuild_cost = 100;
     }));
  (15, Transport ({
       tname = "Coach Shortline Bus";
       towned = false;
       towner = 0;
       tcurrent_rent = 25;
       tprice = 200;
     }));
  (16, Property ({
       pname = "Dairy Bar";
       powned = false;
       powner = 0;
       pcolor = "orange";
       phouses = 0;
       pcurrent_rent = 14;
       prent_list = [14; 70; 200; 500; 750];
       pprice = 180;
       pbuild_cost = 100;
     }));
  (17, CommunityChest);
  (18, Property ({
       pname = "Plant Sciences Building";
       powned = false;
       powner = 0;
       pcolor = "orange";
       phouses = 0;
       pcurrent_rent = 14;
       prent_list = [14; 70; 200; 550; 750];
       pprice = 180;
       pbuild_cost = 100;
     }));
  (19, Property ({
       pname = "Kennedy Hall";
       powned = false;
       powner = 0;
       pcolor = "orange";
       phouses = 0;
       pcurrent_rent = 16;
       prent_list = [16; 80; 220; 600; 800; 1000];
       pprice = 200;
       pbuild_cost = 100;
     }));
  (20, FreeParking); (* Free Parking *)
  (21, Property ({
       pname = "Green Dragon";
       powned = false;
       powner = 0;
       pcolor = "red";
       phouses = 0;
       pcurrent_rent = 18;
       prent_list = [18; 90; 250; 700; 875; 1050];
       pprice = 220;
       pbuild_cost = 150;
     }));
  (22, Chance);
  (23, Property ({
       pname = "Milstein Hall";
       powned = false;
       powner = 0;
       pcolor = "red";
       phouses = 0;
       pcurrent_rent = 18;
       prent_list = [18; 90; 250; 700; 875; 1050];
       pprice = 220;
       pbuild_cost = 150;
     }));
  (24, Property ({
       pname = "Lincoln Hall";
       powned = false;
       powner = 0;
       pcolor = "red";
       phouses = 0;
       pcurrent_rent = 20;
       prent_list = [20; 100; 300; 750; 925; 1100];
       pprice = 240;
       pbuild_cost = 150;
     }));
  (25, Transport ({
       tname = "Big Red Shuttle";
       towned = false;
       towner = 0;
       tcurrent_rent = 25;
       tprice = 200;
     }));
  (26, Property ({
       pname = "Klarman Hall";
       powned = false;
       powner = 0;
       pcolor = "yellow";
       phouses = 0;
       pcurrent_rent = 22;
       prent_list = [22; 110; 330; 800; 975; 1150];
       pprice = 260;
       pbuild_cost = 150;
     }));
  (27, Property ({
       pname = "Physical Sciences Building";
       powned = false;
       powner = 0;
       pcolor = "yellow";
       phouses = 0;
       pcurrent_rent = 22;
       prent_list = [22; 110; 330; 800; 975; 1150];
       pprice = 260;
       pbuild_cost = 150;
     }));
  (28, Utility ({
       uname = "Red Rover Wifi Network";
       uowned = false;
       uowner = 0;
       ucurrent_rent = 25;
       uprice = 150;
     }));
  (29, Property ({
       pname = "Rockefeller Hall";
       powned = false;
       powner = 0;
       pcolor = "yellow";
       phouses = 0;
       pcurrent_rent = 24;
       prent_list = [24; 120; 360; 850; 1025; 1200];
       pprice = 280;
       pbuild_cost = 150;
     }));
  (30, GoToJA);
  (31, Property ({
       pname = "Duffield Hall";
       powned = false;
       powner = 0;
       pcolor = "green";
       phouses = 0;
       pcurrent_rent = 26;
       prent_list = [26; 130; 390; 900; 1100; 1275];
       pprice = 300;
       pbuild_cost = 200;
     }));
  (32, Property ({
       pname = "Carpenter Hall";
       powned = false;
       powner = 0;
       pcolor = "green";
       phouses = 0;
       pcurrent_rent = 26;
       prent_list = [26; 130; 390; 900; 1100; 1275];
       pprice = 300;
       pbuild_cost = 200;
     }));
  (33, CommunityChest);
  (34, Property ({
       pname = "Gates Hall";
       powned = false;
       powner = 0;
       pcolor = "green";
       phouses = 0;
       pcurrent_rent = 28;
       prent_list = [28; 150; 450; 1000; 1200; 1400];
       pprice = 320;
       pbuild_cost = 200;
     }));
  (35, Transport ({
       tname = "Lyft";
       towned = false;
       towner = 0;
       tcurrent_rent = 25;
       tprice = 200;
     }));
  (36, Chance);
  (37, Property ({
       pname = "Sage Hall";
       powned = false;
       powner = 0;
       pcolor = "dark blue";
       phouses = 0;
       pcurrent_rent = 35;
       prent_list = [35; 175; 500; 1100; 1300; 1500];
       pprice = 350;
       pbuild_cost = 200;
     }));
  (38, Tax("Room and Board", 200));
  (39, Property({
       pname = "Statler Hall";
       powned = false;
       powner = 0;
       pcolor = "dark blue";
       phouses = 0;
       pcurrent_rent = 50;
       prent_list = [50; 200; 600; 1400; 1700];
       pprice = 400;
       pbuild_cost = 200;
     }));
  (40, JAsOffice);
]
