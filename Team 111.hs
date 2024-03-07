type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location| B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])
--FINAL
setBoard :: Board
setBoard = (White, [R ('h',1),N ('g',1),B ('f',1),K ('e',1), 
			Q ('d',1),B ('c',1),N ('b',1),R ('a',1),
			P ('h',2),P ('g',2),P ('f',2),P ('e',2),
			P ('d',2),P ('c',2),P ('b',2),P ('a',2)] ,
			[R ('h',8),N ('g',8),B ('f',8),K ('e',8),
			Q ('d',8),B ('c',8),N ('b',8),R ('a',8),
			P ('h',7),P ('g',7),P ('f',7),P ('e',7),
			P ('d',7),P ('c',7),P ('b',7),P ('a',7)])

visualizeBoard :: Board -> String
visualizeBoard b = "   a    b    c    d    e    f    g    h  "++"\n"++visualizeBoard1 b++ "\n" ++ visualizeBoard2 b++ "\n" ++ visualizeBoard3 b++ "\n" ++ visualizeBoard4 b++ "\n" ++ visualizeBoard5 b++ "\n" ++ visualizeBoard6 b++ "\n" ++ visualizeBoard7 b++ "\n" ++ visualizeBoard8 b++"\n"++"\n"++"Turn: " ++getColor b

getColor (p,x,y)|p==Black = "Black"
                |p==White = "White"


visualizeHelper :: Board -> Location -> String
visualizeHelper (p, [], []) (ch,i) = "   | "

visualizeHelper (p, [], (h1:t1)) (ch,i) |visualizeHelper1 h1 == (ch,i) = visualizeHelper2 h1 ++"B | "
										|otherwise = visualizeHelper (p, [], t1) (ch,i)
									
visualizeHelper (p, (h:t), []) (ch,i) 	|visualizeHelper1 h == (ch,i) = visualizeHelper2 h ++"W | "
										|otherwise = visualizeHelper (p, t, []) (ch,i)
									
visualizeHelper (p, (h:t), (h1:t1)) (ch,i)  |visualizeHelper1 h == (ch,i) = visualizeHelper2 h ++"W | "
											|visualizeHelper1 h1 == (ch,i) = visualizeHelper2 h1 ++"B | "
											|otherwise = visualizeHelper (p, t, t1) (ch,i)

visualizeHelper1 :: Piece -> Location

visualizeHelper1 (P (ch,i)) = (ch,i)

visualizeHelper1 (K (ch,i)) = (ch,i)

visualizeHelper1 (N (ch,i)) = (ch,i)

visualizeHelper1 (R (ch,i)) = (ch,i)

visualizeHelper1 (Q (ch,i)) = (ch,i)

visualizeHelper1 (B (ch,i)) = (ch,i)



visualizeHelper2 :: Piece -> String

visualizeHelper2 (P (ch,i)) = "P"

visualizeHelper2 (N (ch,i)) = "N"

visualizeHelper2 (K (ch,i)) = "K"

visualizeHelper2 (R (ch,i)) = "R"

visualizeHelper2 (B (ch,i)) = "B"

visualizeHelper2 (Q (ch,i)) = "Q"


visualizeBoard1 board="8| "++visualizeHelper board('a',8)++visualizeHelper board('b',8)++visualizeHelper board('c',8)++visualizeHelper board('d',8)++visualizeHelper board('e',8)++visualizeHelper board('f',8)++visualizeHelper board('g',8)++visualizeHelper board('h',8)
visualizeBoard2 board="7| "++visualizeHelper board('a',7)++visualizeHelper board('b',7)++visualizeHelper board('c',7)++visualizeHelper board('d',7)++visualizeHelper board('e',7)++visualizeHelper board('f',7)++visualizeHelper board('g',7)++visualizeHelper board('h',7)
visualizeBoard3 board="6| "++visualizeHelper board('a',6)++visualizeHelper board('b',6)++visualizeHelper board('c',6)++visualizeHelper board('d',6)++visualizeHelper board('e',6)++visualizeHelper board('f',6)++visualizeHelper board('g',6)++visualizeHelper board('h',6)
visualizeBoard4 board="5| "++visualizeHelper board('a',5)++visualizeHelper board('b',5)++visualizeHelper board('c',5)++visualizeHelper board('d',5)++visualizeHelper board('e',5)++visualizeHelper board('f',5)++visualizeHelper board('g',5)++visualizeHelper board('h',5)
visualizeBoard5 board="4| "++visualizeHelper board('a',4)++visualizeHelper board('b',4)++visualizeHelper board('c',4)++visualizeHelper board('d',4)++visualizeHelper board('e',4)++visualizeHelper board('f',4)++visualizeHelper board('g',4)++visualizeHelper board('h',4)
visualizeBoard6 board="3| "++visualizeHelper board('a',3)++visualizeHelper board('b',3)++visualizeHelper board('c',3)++visualizeHelper board('d',3)++visualizeHelper board('e',3)++visualizeHelper board('f',3)++visualizeHelper board('g',3)++visualizeHelper board('h',3)
visualizeBoard7 board="2| "++visualizeHelper board('a',2)++visualizeHelper board('b',2)++visualizeHelper board('c',2)++visualizeHelper board('d',2)++visualizeHelper board('e',2)++visualizeHelper board('f',2)++visualizeHelper board('g',2)++visualizeHelper board('h',2)
visualizeBoard8 board="1| "++visualizeHelper board('a',1)++visualizeHelper board('b',1)++visualizeHelper board('c',1)++visualizeHelper board('d',1)++visualizeHelper board('e',1)++visualizeHelper board('f',1)++visualizeHelper board('g',1)++visualizeHelper board('h',1)
			

checkPawn p1 [] = False
 
checkPawn p1 (wh:wt)  | p1==wh = True
					  | otherwise = checkPawn p1 wt 

isPresent [] _ = False
isPresent  ( ( P(c,i)):t) (nc,ni)| c==nc && i==ni = True
							   | otherwise = isPresent t (nc,ni)
							   
isPresent  ( ( N(c,i)):t) (nc,ni)| c==nc && i==ni = True
							   | otherwise = isPresent t (nc,ni)
							 
isPresent  ( ( K(c,i)):t) (nc,ni)| c==nc && i==ni = True
							   | otherwise = isPresent t (nc,ni)
							  
isPresent  ( ( Q(c,i)):t) (nc,ni)| c==nc && i==ni = True
							   | otherwise = isPresent t (nc,ni)
							  
isPresent  ( ( R(c,i)):t) (nc,ni)| c==nc && i==ni = True
							   | otherwise = isPresent t (nc,ni)
							  
isPresent  ( ( B(c,i)):t) (nc,ni)| c==nc && i==ni = True
							   | otherwise = isPresent t (nc,ni)
							   
translate :: Char ->Int
translate char| char=='a' = 1
			  | char=='b' = 2
			  | char=='c' = 3
			  | char=='d' = 4
			  | char=='e' = 5
			  | char=='f' = 6
			  | char=='g' = 7	
			  | char=='h' = 8

--checks if picece in your way(y-axis)							   
checkPieceinWay []_ _ _ = False						   
checkPieceinWay ( ( P(c,ref)):t) nc  i  ni | i>ni && c==nc && ref<i && ref>ni  = True
										   | i<ni && c==nc && ref>i && ref<ni = True
										   | otherwise = checkPieceinWay t nc i ni
										   
checkPieceinWay ( ( N(c,ref)):t) nc  i  ni | i>ni && c==nc && ref<i && ref>ni  = True
										   | i<ni && c==nc && ref>i && ref<ni = True
										   | otherwise = checkPieceinWay t nc i ni
										   
checkPieceinWay ( ( Q(c,ref)):t) nc  i  ni | i>ni && c==nc && ref<i && ref>ni  = True
										   | i<ni && c==nc && ref>i && ref<ni = True
										   | otherwise = checkPieceinWay t nc i ni
										   
checkPieceinWay ( ( K(c,ref)):t) nc  i  ni | i>ni && c==nc && ref<i && ref>ni  = True
										   | i<ni && c==nc && ref>i && ref<ni = True
										   | otherwise = checkPieceinWay t nc i ni	
										   
checkPieceinWay ( ( R(c,ref)):t) nc  i  ni | i>ni && c==nc && ref<i && ref>ni  = True
										   | i<ni && c==nc && ref>i && ref<ni = True
										   | otherwise = checkPieceinWay t nc i ni

checkPieceinWay ( ( B(c,ref)):t) nc  i  ni | i>ni && c==nc && ref<i && ref>ni  = True
										   | i<ni && c==nc && ref>i && ref<ni = True
										   | otherwise = checkPieceinWay t nc i ni

--checks if picece in your way(x-axis)
checkPieceinWayX []_ _ _ = False	
checkPieceinWayX ( ( P(ref,i)):t) c  nc  ni | i==ni && ((translate ref)>(translate c)) && ((translate ref)<(translate nc)) && ((translate ref)/=(translate nc)) = True
										    | i==ni && ((translate ref)<(translate c)) && ((translate ref)>(translate nc)) && ((translate ref)/=(translate nc)) = True
										    | otherwise = checkPieceinWayX t c nc ni
										   
checkPieceinWayX ( ( N(ref,i)):t) c  nc  ni | i==ni && ((translate ref)>(translate c)) && ((translate ref)<(translate nc)) && ((translate ref)/=(translate nc)) = True
											| i==ni && ((translate ref)<(translate c)) && ((translate ref)>(translate nc)) && ((translate ref)/=(translate nc)) = True
										    | otherwise = checkPieceinWayX t c nc ni
										   
checkPieceinWayX ( ( Q(ref,i)):t) c  nc  ni | i==ni && ((translate ref)>(translate c)) && ((translate ref)<(translate nc)) && ((translate ref)/=(translate nc)) = True
											| i==ni && ((translate ref)<(translate c)) && ((translate ref)>(translate nc)) && ((translate ref)/=(translate nc)) = True
										    | otherwise = checkPieceinWayX t c nc ni
										   
checkPieceinWayX ( ( K(ref,i)):t) c  nc  ni | i==ni && ((translate ref)>(translate c)) && ((translate ref)<(translate nc)) && ((translate ref)/=(translate nc)) = True
											| i==ni && ((translate ref)<(translate c)) && ((translate ref)>(translate nc)) && ((translate ref)/=(translate nc)) = True
										    | otherwise = checkPieceinWayX t c nc ni
										   
checkPieceinWayX ( ( R(ref,i)):t) c  nc  ni | i==ni && ((translate ref)>(translate c)) && ((translate ref)<(translate nc)) && ((translate ref)/=(translate nc)) = True
											| i==ni && ((translate ref)<(translate c)) && ((translate ref)>(translate nc)) && ((translate ref)/=(translate nc)) = True
										    | otherwise = checkPieceinWayX t c nc ni

checkPieceinWayX ( ( B(ref,i)):t) c  nc  ni | i==ni && ((translate ref)>(translate c)) && ((translate ref)<(translate nc)) && ((translate ref)/=(translate nc)) = True
											| i==ni && ((translate ref)<(translate c)) && ((translate ref)>(translate nc)) && ((translate ref)/=(translate nc)) = True
										    | otherwise = checkPieceinWayX t c nc ni

--check if thers a picec in the way 
checkPieceinWayD:: [Piece] -> (Char,Int)-> (Char,Int)-> Bool 

checkPieceinWayD list (c,i) (nc,ni) | (abs(((translate nc) - (translate c)))==1) && (abs(ni - i)==1) = False 
									| ((ni-i)>0) && (((translate nc) - (translate c))>0) && not (isPresent list ((succ c),(i+1))) = checkPieceinWayD list ((succ c),(i+1)) (nc,ni) 
								    | ((ni-i)>0) && (((translate nc) - (translate c))<0) && not (isPresent list ((pred c),(i+1))) = checkPieceinWayD list ((pred c),(i+1)) (nc,ni)
									| ((ni-i)<0) && (((translate nc) - (translate c))>0) && not (isPresent list ((succ c),(i-1))) = checkPieceinWayD list ((succ c),(i-1)) (nc,ni)
									| ((ni-i)<0) && (((translate nc) - (translate c))<0) && not (isPresent list ((pred c),(i-1))) = checkPieceinWayD list ((pred c),(i-1)) (nc,ni)
									| otherwise= True

								   
isDiag :: (Int, Int) -> (Int, Int) -> Bool
isDiag (x1, y1) (x2, y2) = abs(x2 - x1) == abs(y2 - y1)			  

checkColor:: Piece->[Piece]->[Piece]->Player

checkColor p [] (bh:bt)= Black
checkColor p (wh:wt) []= White
checkColor p (wh:wt) (bh:bt)| p==wh = White
							| p==bh = Black
							| otherwise= checkColor p wt bt
 
										   
isLegal:: Piece -> Board -> Location -> Bool
 

isLegal (P (c,i)) (player,whiteList,blackList) (nc,ni) | (checkColor (P (c,i)) whiteList blackList)==White && ( (i-ni==1 || ni-i==1) || (i-ni==2 || ni-i==2) )  && (i==2) &&(nc==c) && not (isPresent whiteList (nc,ni)) && not (isPresent blackList (nc,ni) ) && not ( checkPieceinWay whiteList c i ni ) && not ( checkPieceinWay blackList c i ni )  && (i<ni)    = True
												   | (checkColor (P (c,i)) whiteList blackList)==Black && ( (i-ni==1 || ni-i==1) || (i-ni==2 || ni-i==2) )  && (i==7) &&(nc==c)&& not (isPresent whiteList (nc,ni)) && not (isPresent blackList (nc,ni) ) && not ( checkPieceinWay whiteList c i ni ) && not ( checkPieceinWay blackList c i ni )  && (ni<i)    = True
												   | (checkColor (P (c,i)) whiteList blackList)==White && (i-ni==1 || ni-i==1)  && (i/=2) && not (isPresent whiteList (nc,ni)) && not( isPresent blackList (nc,ni) ) && (i<ni) &&(nc==c)   = True
												   | (checkColor (P (c,i)) whiteList blackList)==Black && (i-ni==1 || ni-i==1) &&  (i/=7) && not (isPresent blackList (nc,ni)) && not( isPresent whiteList (nc,ni) ) && (ni<i) &&(nc==c)   = True
												   | (checkColor (P (c,i)) whiteList blackList)==White && (isPresent blackList (nc,ni))&& (i-ni==1 || ni-i==1) && (pred c== nc || succ c== nc) && (i<ni)  = True
												   | (checkColor (P (c,i)) whiteList blackList)==Black && (isPresent whiteList (nc,ni))&& (i-ni==1 || ni-i==1) && (pred c== nc || succ c== nc) && (ni<i)  = True
												   | otherwise = False
												   
isLegal (R(c,i)) (player,whiteList, blackList) (nc,ni) | (checkColor (R (c,i)) whiteList blackList) == White && (ni == i) && (nc/=c) && not(checkPieceinWayX (whiteList++blackList) c nc ni) && not (isPresent whiteList (nc,ni)) = True
													   | (checkColor (R (c,i)) whiteList blackList) == Black && (ni == i) && (nc/=c)&& not(checkPieceinWayX (whiteList++blackList) c nc ni)  && not (isPresent blackList (nc,ni)) = True
													   | (checkColor (R (c,i)) whiteList blackList) == White && (nc == c) && (ni/=i)&& not(checkPieceinWay  (whiteList++blackList) c i ni)   && not (isPresent whiteList (nc,ni)) = True
													   | (checkColor (R (c,i)) whiteList blackList) == Black && (nc == c) && (ni/=i)&& not(checkPieceinWay  (whiteList++blackList) c i ni)   && not (isPresent blackList (nc,ni))= True
													   | otherwise = False
												   
isLegal (K(c,i)) (player,whiteList,blackList) (nc,ni)  | (checkColor (K (c,i)) whiteList blackList)==White && (i-ni==1 || ni-i==1) && (nc==c) && not (isPresent whiteList (nc,ni) )    = True
												   | (checkColor (K (c,i)) whiteList blackList)==Black && (i-ni==1 || ni-i==1) && (nc==c) && not (isPresent blackList (nc,ni) )    = True
												   | (checkColor (K (c,i)) whiteList blackList)==White && (i==ni) && (pred c== nc || succ c== nc) && not (isPresent whiteList (nc,ni) )    = True
												   | (checkColor (K (c,i)) whiteList blackList)==Black && (i==ni) && (pred c== nc || succ c== nc) && not (isPresent blackList (nc,ni) )    = True
												   | (checkColor (K (c,i)) whiteList blackList)==White && (i-ni==1 || ni-i==1) && (pred c== nc || succ c== nc) &&   not (isPresent whiteList (nc,ni))   = True
												   | (checkColor (K (c,i)) whiteList blackList)==Black && (i-ni==1 || ni-i==1) && (pred c== nc || succ c== nc) &&   not (isPresent blackList (nc,ni))   = True 
												   | otherwise= False

isLegal (N(c,i)) (player,whiteList,blackList) (nc,ni)  | (checkColor (N (c,i)) whiteList blackList)==White && (nc== pred c || nc==succ c) && (ni==i+2 || ni==i-2) && not(isPresent whiteList (nc,ni)) = True
												   | (checkColor (N (c,i)) whiteList blackList)==Black && (nc== pred c || nc==succ c) && (ni==i+2 || ni==i-2) && not(isPresent blackList (nc,ni)) = True
												   | (checkColor (N (c,i)) whiteList blackList)==White && (nc== pred(pred c) || nc==succ(succ c)) && (ni==i+1 || ni==i-1) && not(isPresent whiteList (nc,ni)) = True
												   | (checkColor (N (c,i)) whiteList blackList)==Black && (nc== pred(pred c) || nc==succ(succ c)) && (ni==i+1 || ni==i-1) && not(isPresent blackList (nc,ni)) = True
												   | otherwise= False

isLegal (B(c,i)) (player,whiteList,blackList) (nc,ni)  | (checkColor (B (c,i)) whiteList blackList)==White  && (isDiag ((translate c), i) ((translate nc), ni)) && not (checkPieceinWayD whiteList (c,i) (nc,ni)) && not (checkPieceinWayD blackList (c,i) (nc,ni)) && not (isPresent whiteList (nc,ni))  = True
												   | (checkColor (B (c,i)) whiteList blackList)==Black  && (isDiag ((translate c), i) ((translate nc), ni)) && not (checkPieceinWayD whiteList (c,i) (nc,ni)) && not (checkPieceinWayD blackList (c,i) (nc,ni)) && not (isPresent blackList (nc,ni))      = True
												   | otherwise= False	
												   
isLegal (Q(c,i)) (player,whiteList,blackList) (nc,ni)| (checkColor (Q (c,i)) whiteList blackList)==White  && (isDiag ((translate c), i) ((translate nc), ni)) && not (checkPieceinWayD whiteList (c,i) (nc,ni)) && not (checkPieceinWayD blackList (c,i) (nc,ni)) && not (isPresent whiteList (nc,ni)) && not (isPresent whiteList (nc,ni))  = True
												   | (checkColor (Q (c,i)) whiteList blackList)==Black  && (isDiag ((translate c), i) ((translate nc), ni)) && not (checkPieceinWayD whiteList (c,i) (nc,ni)) && not (checkPieceinWayD blackList (c,i) (nc,ni)) && not (isPresent blackList (nc,ni)) && not (isPresent blackList (nc,ni)) = True
												   | (checkColor (Q (c,i)) whiteList blackList) == White && (ni == i) && (nc/=c) && not(checkPieceinWayX whiteList c nc ni) && not(checkPieceinWayX blackList c nc ni) && not (isPresent whiteList (nc,ni)) = True
											       | (checkColor (Q (c,i)) whiteList blackList) == Black && (ni == i) && (nc/=c)&& not(checkPieceinWayX whiteList c nc ni) && not(checkPieceinWayX blackList c nc ni) && not (isPresent blackList (nc,ni)) = True												   
												   | (checkColor (Q (c,i)) whiteList blackList) == White && (nc == c) && (ni/=i)&& not(checkPieceinWay  whiteList c i ni)  && not(checkPieceinWay blackList c i ni) && not (isPresent whiteList (nc,ni)) = True
												   | (checkColor (Q (c,i)) whiteList blackList) == Black && (nc == c) && (ni/=i)&& not(checkPieceinWay  whiteList c i ni)  && not(checkPieceinWay blackList c i ni) && not (isPresent blackList (nc,ni))= True
												   | otherwise= False


suggestMove :: Piece-> Board-> [Location]

suggestMove	(P (c,i)) board= (suggestHelper (P (c,i))  board ('a',8) )
suggestMove	(R (c,i)) board= (suggestHelper (R (c,i))  board ('a',8) )
suggestMove	(K (c,i)) board= (suggestHelper (K (c,i))  board ('a',8) )
suggestMove	(Q (c,i)) board= (suggestHelper (Q (c,i))  board ('a',8) )
suggestMove	(N (c,i)) board= (suggestHelper (N (c,i))  board ('a',8) )
suggestMove	(B (c,i)) board= (suggestHelper (B (c,i))  board ('a',8) )


suggestHelper:: Piece-> Board-> Location->[Location]

suggestHelper (P (c,i)) board ('h',1)=[]
suggestHelper (P (c,i)) board (cc,ci)| ci>1 && (translate cc)<=8 && (isLegal (P (c,i)) board (cc,ci))= (cc,ci):( suggestHelper (P (c,i)) board (cc,(ci-1)) )
									 | ci==1 && (translate cc)<=8 && (isLegal (P (c,i)) board (cc,ci))= (cc,ci):(suggestHelper (P (c,i)) board ((succ cc),8))
									 | ci>1 && (translate cc)<=8 && not (isLegal (P (c,i)) board (cc,ci))= suggestHelper (P (c,i)) board (cc,(ci-1))
									 | otherwise= (suggestHelper (P (c,i)) board ((succ cc),8))
									-- | otherwise= suggestHelper (P (c,i)) board ((succ cc),8)
									 
suggestHelper (R (c,i)) board ('h',1)=[]
suggestHelper (R (c,i)) board (cc,ci) | ci>1 && (translate cc)<=8 && (isLegal (R (c,i)) board (cc,ci))= (cc,ci):( suggestHelper (R (c,i)) board (cc,(ci-1)) )
									 | ci==1 && (translate cc)<=8 && (isLegal (R (c,i)) board (cc,ci))= (cc,ci):(suggestHelper (R (c,i)) board ((succ cc),8))
									 | ci>1 && (translate cc)<=8 && not (isLegal (R (c,i)) board (cc,ci))= suggestHelper (R (c,i)) board (cc,(ci-1))
									 | otherwise= (suggestHelper (R (c,i)) board ((succ cc),8))					
									 -- | otherwise= suggestHelper (R (c,i)) board ((succ cc),8)
									 
suggestHelper (K (c,i)) board ('h',1)=[]
suggestHelper (K (c,i)) board (cc,ci) | ci>1 && (translate cc)<=8 && (isLegal (K (c,i)) board (cc,ci))= (cc,ci):( suggestHelper (K (c,i)) board (cc,(ci-1)) )
									 | ci==1 && (translate cc)<=8 && (isLegal (K (c,i)) board (cc,ci))= (cc,ci):(suggestHelper (K (c,i)) board ((succ cc),8))
									 | ci>1 && (translate cc)<=8 && not (isLegal (K (c,i)) board (cc,ci))= suggestHelper (K (c,i)) board (cc,(ci-1))
									--  | ci==1 && (translate cc)<=8 && not (isLegal (K (c,i)) board (cc,ci))= (suggestHelper (K (c,i)) board ((succ cc),8))
									 |otherwise= (suggestHelper (K (c,i)) board ((succ cc),8))
									 
									 
suggestHelper (Q (c,i)) board ('h',1)=[]
suggestHelper (Q (c,i)) board (cc,ci) | ci>1 && (translate cc)<=8 && (isLegal (Q (c,i)) board (cc,ci))= (cc,ci):( suggestHelper (Q (c,i)) board (cc,(ci-1)) )
									 | ci==1 && (translate cc)<=8 && (isLegal (Q (c,i)) board (cc,ci))= (cc,ci):(suggestHelper (Q (c,i)) board ((succ cc),8))
									 | ci>1 && (translate cc)<=8 && not (isLegal (Q (c,i)) board (cc,ci))= suggestHelper (Q (c,i)) board (cc,(ci-1))
									--  | ci==1 && (translate cc)<=8 && not (isLegal (Q (c,i)) board (cc,ci))= (suggestHelper (Q (c,i)) board ((succ cc),8))
									| otherwise= (suggestHelper (Q (c,i)) board ((succ cc),8))

suggestHelper (N (c,i)) board ('h',1)=[]
suggestHelper (N (c,i)) board (cc,ci) | ci>1 && (translate cc)<=8 && (isLegal (N (c,i)) board (cc,ci))= (cc,ci):( suggestHelper (N (c,i)) board (cc,(ci-1)) )
									 | ci==1 && (translate cc)<=8 && (isLegal (N (c,i)) board (cc,ci))= (cc,ci):(suggestHelper (N (c,i)) board ((succ cc),8))
									 | ci>1 && (translate cc)<=8 && not (isLegal (N (c,i)) board (cc,ci))= suggestHelper (N (c,i)) board (cc,(ci-1))
									 -- | ci==1 && (translate cc)<=8 && not (isLegal (N (c,i)) board (cc,ci))= (suggestHelper (N (c,i)) board ((succ cc),8))
									| otherwise= (suggestHelper (N (c,i)) board ((succ cc),8))
									 
suggestHelper (B (c,i)) board ('h',1)=[]
suggestHelper (B (c,i)) board (cc,ci) | ci>1 && (translate cc)<=8 && (isLegal (B (c,i)) board (cc,ci))= (cc,ci):( suggestHelper (B (c,i)) board (cc,(ci-1)) )
									 | ci==1 && (translate cc)<=8 && (isLegal (B (c,i)) board (cc,ci))= (cc,ci):(suggestHelper (B (c,i)) board ((succ cc),8))
									 | ci>1 && (translate cc)<=8 && not (isLegal (B (c,i)) board (cc,ci))= suggestHelper (B (c,i)) board (cc,(ci-1))
								--	  | ci==1 && (translate cc)<=8 && not (isLegal (B (c,i)) board (cc,ci))= (suggestHelper (B (c,i)) board ((succ cc),8))
									 | otherwise= (suggestHelper (B (c,i)) board ((succ cc),8))

replacePos _ [] _ = []

replacePos (P(c,i)) (h:t) (nc,ni)| h==(P(c,i)) = P(nc,ni):replacePos (P(c,i)) t (nc,ni)
								 | h==(P(nc,ni))= replacePos (P(c,i)) t (nc,ni)
								 | otherwise = h:replacePos (P(c,i)) t (nc,ni)
								 
replacePos (R(c,i)) (h:t) (nc,ni)| h==(R(c,i)) =   R(nc,ni):replacePos (R(c,i)) (t) (nc,ni)
								 | h==(R(nc,ni))= replacePos (R(c,i)) t (nc,ni)
								 | otherwise = h:replacePos (R(c,i)) t (nc,ni)
								 
replacePos (K(c,i)) (h:t) (nc,ni)|h==(K(c,i)) = K(nc,ni):replacePos (K(c,i)) (t) (nc,ni)
								 | h==(K(nc,ni))= replacePos (K(c,i)) t (nc,ni)
								 | otherwise = h:replacePos (K(c,i)) t (nc,ni)
								 
replacePos (N(c,i)) (h:t) (nc,ni)| h==(N(c,i)) = N(nc,ni):replacePos (N(c,i)) (t) (nc,ni)
								 |  h==(N(nc,ni))= replacePos (N(c,i)) t (nc,ni)
								 | otherwise = h:replacePos (N(c,i)) t (nc,ni)
								 
replacePos (Q(c,i)) (h:t) (nc,ni)| h==(Q(c,i)) = Q(nc,ni):replacePos (Q(c,i)) (t) (nc,ni)
								 | h==(Q(nc,ni))= replacePos (Q(c,i)) t (nc,ni)
								 | otherwise = h:replacePos (Q(c,i)) t (nc,ni)
								 
replacePos (B(c,i)) (h:t) (nc,ni)|  h==(B(c,i)) = B(nc,ni):replacePos (B(c,i)) (t) (nc,ni)
								 |  h==(B(nc,ni))= replacePos (B(c,i)) t (nc,ni)
								 | otherwise = h:replacePos (B(c,i)) t (nc,ni)
								 
removePos [] _= []
removePos ((P(c,i)):t) (nc,ni)| nc==c && i==ni= removePos t (nc,ni)
							  | otherwise= (P(c,i)):removePos t (nc,ni)
							  
removePos ((R(c,i)):t) (nc,ni)| nc==c && i==ni= removePos t (nc,ni)
							  |  otherwise=(R(c,i)):removePos t (nc,ni)
							  
removePos ((K(c,i)):t) (nc,ni)| nc==c && i==ni= removePos t (nc,ni)
							  |  otherwise=(K(c,i)):removePos t (nc,ni)

removePos ((N(c,i)):t) (nc,ni)| nc==c && i==ni= removePos t (nc,ni)
							  |  otherwise=(N(c,i)):removePos t (nc,ni)
							  
removePos ((Q(c,i)):t) (nc,ni)| nc==c && i==ni= removePos t (nc,ni)
							  |  otherwise=(Q(c,i)):removePos t (nc,ni)

removePos ((B(c,i)):t) (nc,ni)| nc==c && i==ni= removePos t (nc,ni)
							  |  otherwise=(B(c,i)):removePos t (nc,ni)							  
							  
							  

move:: Piece -> Location -> Board -> Board 	

move (P(c,i)) (nc,ni) (player,whiteList,blackList)| (checkColor (P (c,i)) whiteList blackList)==White && player==White && (isLegal (P(c,i)) (player,whiteList,blackList) (nc,ni))=(Black,(replacePos (P(c,i)) whiteList (nc,ni)),(removePos blackList (nc,ni)))
												  | (checkColor (P (c,i)) whiteList blackList)==Black && player==Black && (isLegal (P(c,i)) (player,whiteList,blackList) (nc,ni))=(White,(removePos whiteList (nc,ni)),(replacePos (P(c,i)) blackList (nc,ni)))
												  | (checkColor (P (c,i)) whiteList blackList)==White && player==Black= error(" This is Black player’s turn, White can’t move.")
												  | (checkColor (P (c,i)) whiteList blackList)==Black && player==White= error(" This is White player’s turn, Black can’t move.")
												  | otherwise = error("illegal move for piece P"++"("++show(c)++","++show(i)++")")
												  
move (R(c,i)) (nc,ni) (player,whiteList,blackList)| (checkColor (R(c,i)) whiteList blackList)==White && player==White && (isLegal (R(c,i)) (player,whiteList,blackList) (nc,ni))=(Black,(replacePos (R(c,i)) whiteList (nc,ni)),(removePos blackList (nc,ni)))
												  | (checkColor (R (c,i)) whiteList blackList)==Black && player==Black && (isLegal (R(c,i)) (player,whiteList,blackList) (nc,ni))=(White,(removePos whiteList (nc,ni)),(replacePos (R(c,i)) blackList (nc,ni)))
												  | (checkColor (R (c,i)) whiteList blackList)==White && player==Black= error(" This is Black player’s turn, White can’t move.")
												  | (checkColor (R (c,i)) whiteList blackList)==Black && player==White= error(" This is White player’s turn, Black can’t move.")
												  | otherwise = error("illegal move for piece R"++"("++show(c)++","++show(i)++")")
												  
move (K(c,i)) (nc,ni) (player,whiteList,blackList)| (checkColor (K(c,i)) whiteList blackList)==White && player==White && (isLegal (K(c,i)) (player,whiteList,blackList) (nc,ni))=(Black,(replacePos (K(c,i)) whiteList (nc,ni)),(removePos blackList (nc,ni)))
												  | (checkColor (K(c,i)) whiteList blackList)==Black && player==Black && (isLegal (K(c,i)) (player,whiteList,blackList) (nc,ni))=(White,(removePos whiteList (nc,ni)),(replacePos (K(c,i)) blackList (nc,ni)))
												  | (checkColor (K(c,i)) whiteList blackList)==White && player==Black= error(" This is Black player’s turn, White can’t move.")
												  | (checkColor (K(c,i)) whiteList blackList)==Black && player==White= error(" This is White player’s turn, Black can’t move.")
												  | otherwise = error("illegal move for piece K"++"("++show(c)++","++show(i)++")")
												  
move (N(c,i)) (nc,ni) (player,whiteList,blackList)| (checkColor (N(c,i)) whiteList blackList)==White && player==White && (isLegal (N(c,i)) (player,whiteList,blackList) (nc,ni))=(Black,(replacePos (N(c,i)) whiteList (nc,ni)),(removePos blackList (nc,ni)))
												  | (checkColor (N(c,i)) whiteList blackList)==Black && player==Black && (isLegal (N(c,i)) (player,whiteList,blackList) (nc,ni))=(White,(removePos whiteList (nc,ni)),(replacePos (N(c,i)) blackList (nc,ni)))
												  | (checkColor (N(c,i)) whiteList blackList)==White && player==Black= error(" This is Black player’s turn, White can’t move.")
												  | (checkColor (N(c,i)) whiteList blackList)==Black && player==White= error(" This is White player’s turn, Black can’t move.")
												  | otherwise = error("illegal move for piece N"++"("++show(c)++","++show(i)++")")
												  
move (Q(c,i)) (nc,ni) (player,whiteList,blackList)| (checkColor (Q(c,i)) whiteList blackList)==White && player==White && (isLegal (Q(c,i)) (player,whiteList,blackList) (nc,ni))=(Black,(replacePos (Q(c,i)) whiteList (nc,ni)),(removePos blackList (nc,ni)))
												  | (checkColor (Q(c,i)) whiteList blackList)==Black && player==Black && (isLegal (Q(c,i)) (player,whiteList,blackList) (nc,ni))=(White,(removePos whiteList (nc,ni)),(replacePos (Q(c,i)) blackList (nc,ni)))
												  | (checkColor (Q(c,i)) whiteList blackList)==White && player==Black= error(" This is Black player’s turn, White can’t move.")
												  | (checkColor (Q(c,i)) whiteList blackList)==Black && player==White= error(" This is White player’s turn, Black can’t move.")
												  | otherwise = error("illegal move for piece Q"++"("++show(c)++","++show(i)++")")	

move (B(c,i)) (nc,ni) (player,whiteList,blackList)| (checkColor (B(c,i)) whiteList blackList)==White && player==White && (isLegal (B(c,i)) (player,whiteList,blackList) (nc,ni))=(Black,(replacePos (B(c,i)) whiteList (nc,ni)),(removePos blackList (nc,ni)))
												  | (checkColor (B(c,i)) whiteList blackList)==Black && player==Black && (isLegal (B(c,i)) (player,whiteList,blackList) (nc,ni))=(White,(removePos whiteList (nc,ni)),(replacePos (B(c,i)) blackList (nc,ni)))
												  | (checkColor (B(c,i)) whiteList blackList)==White && player==Black= error(" This is Black player’s turn, White can’t move.")
												  | (checkColor (B(c,i)) whiteList blackList)==Black && player==White= error(" This is White player’s turn, Black can’t move.")
												  | otherwise = error("illegal move for piece B"++"("++show(c)++","++show(i)++")")												  
												  