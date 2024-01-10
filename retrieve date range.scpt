FasdUAS 1.101.10   ��   ��    k             l     ��������  ��  ��        l     ��������  ��  ��     	 
 	 l     ��������  ��  ��   
     l     ��  ��    G A Function to prompt for date input and return it as a date object     �   �   F u n c t i o n   t o   p r o m p t   f o r   d a t e   i n p u t   a n d   r e t u r n   i t   a s   a   d a t e   o b j e c t      i         I      �� ���� 0 promptfordate promptForDate   ��  o      ���� 0 promptmessage promptMessage��  ��    k     �       l     ��  ��      Get the current date     �   *   G e t   t h e   c u r r e n t   d a t e      r         l    	  ����   \     	 ! " ! l     #���� # I    ������
�� .misccurdldt    ��� null��  ��  ��  ��   " ]     $ % $ m    ����  % 1    ��
�� 
days��  ��    o      ���� 0 	firstdate 	firstDate   & ' & r     ( ) ( c     * + * n     , - , 1    ��
�� 
day  - o    ���� 0 	firstdate 	firstDate + m    ��
�� 
nmbr ) o      ���� 0 
currentday 
currentDay '  . / . r     0 1 0 c     2 3 2 n     4 5 4 m    ��
�� 
mnth 5 o    ���� 0 	firstdate 	firstDate 3 m    ��
�� 
nmbr 1 o      ���� 0 currentmonth currentMonth /  6 7 6 r    # 8 9 8 c    ! : ; : n     < = < 1    ��
�� 
year = o    ���� 0 	firstdate 	firstDate ; m     ��
�� 
nmbr 9 o      ���� 0 currentyear currentYear 7  > ? > l  $ $��������  ��  ��   ?  @ A @ l  $ $�� B C��   B * $ Format the current date as MMDDYYYY    C � D D H   F o r m a t   t h e   c u r r e n t   d a t e   a s   M M D D Y Y Y Y A  E F E r   $ 7 G H G c   $ 5 I J I b   $ 3 K L K b   $ 1 M N M n  $ * O P O I   % *�� Q���� 0 zeropad zeroPad Q  R�� R o   % &���� 0 currentmonth currentMonth��  ��   P  f   $ % N n  * 0 S T S I   + 0�� U���� 0 zeropad zeroPad U  V�� V o   + ,���� 0 
currentday 
currentDay��  ��   T  f   * + L o   1 2���� 0 currentyear currentYear J m   3 4��
�� 
TEXT H o      ���� 0 defaultdate defaultDate F  W X W l  8 8��������  ��  ��   X  Y Z Y r   8 C [ \ [ n   8 A ] ^ ] 1   ? A��
�� 
ttxt ^ l  8 ? _���� _ I  8 ?�� ` a
�� .sysodlogaskr        TEXT ` o   8 9���� 0 promptmessage promptMessage a �� b��
�� 
dtxt b o   : ;���� 0 defaultdate defaultDate��  ��  ��   \ o      ���� 0 	inputdate 	inputDate Z  c d c l  D D��������  ��  ��   d  e f e l  D D�� g h��   g !  Length check for the input    h � i i 6   L e n g t h   c h e c k   f o r   t h e   i n p u t f  j k j Z   D i l m���� l >  D I n o n n   D G p q p 1   E G��
�� 
leng q o   D E���� 0 	inputdate 	inputDate o m   G H����  m k   L e r r  s t s I  L ]�� u v
�� .sysodlogaskr        TEXT u m   L M w w � x x z I n v a l i d   d a t e .   P l e a s e   e n t e r   8   d i g i t s   f o r   t h e   d a t e   a s   M M D D Y Y Y Y . v �� y z
�� 
btns y J   N S { {  |�� | m   N Q } } � ~ ~  O K��   z �� ��
�� 
dflt  m   V W���� ��   t  ��� � l  ^ e � � � � L   ^ e � � I   ^ d�� ����� 0 promptfordate promptForDate �  ��� � o   _ `���� 0 promptmessage promptMessage��  ��   � B < Recursively call the function until valid input is provided    � � � � x   R e c u r s i v e l y   c a l l   t h e   f u n c t i o n   u n t i l   v a l i d   i n p u t   i s   p r o v i d e d��  ��  ��   k  � � � l  j j��������  ��  ��   �  � � � l  j j�� � ���   � * $ Convert the string to a date object    � � � � H   C o n v e r t   t h e   s t r i n g   t o   a   d a t e   o b j e c t �  � � � r   j y � � � n   j w � � � 7  k w�� � �
�� 
ctxt � m   q s����  � m   t v����  � o   j k���� 0 	inputdate 	inputDate � o      ���� 0 daypart dayPart �  � � � r   z � � � � n   z � � � � 7  { ��� � �
�� 
ctxt � m   � �����  � m   � �����  � o   z {���� 0 	inputdate 	inputDate � o      ���� 0 	monthpart 	monthPart �  � � � r   � � � � � n   � � � � � 7  � ��� � �
�� 
ctxt � m   � �����  � m   � �����  � o   � ����� 0 	inputdate 	inputDate � o      ���� 0 yearpart yearPart �  � � � r   � � � � � b   � � � � � b   � � � � � b   � � � � � b   � � � � � o   � ����� 0 daypart dayPart � m   � � � � � � �  / � o   � ����� 0 	monthpart 	monthPart � m   � � � � � � �  / � o   � ����� 0 yearpart yearPart � o      ���� *0 formatteddatestring formattedDateString �  � � � l  � ���������  ��  ��   �  ��� � L   � � � � 4   � ��� �
�� 
ldt  � o   � ����� *0 formatteddatestring formattedDateString��     � � � l     ��������  ��  ��   �  � � � l     �� � ���   � = 7 Function to generate all dates between two given dates    � � � � n   F u n c t i o n   t o   g e n e r a t e   a l l   d a t e s   b e t w e e n   t w o   g i v e n   d a t e s �  � � � i     � � � I      �� ����� 0 generatedates generateDates �  � � � o      ���� 0 	startdate 	startDate �  ��� � o      ���� 0 enddate endDate��  ��   � k     % � �  � � � r      � � � o     ���� 0 	startdate 	startDate � o      ���� 0 currentdate currentDate �  � � � r     � � � J    ����   � o      ���� 0 datelist dateList �  � � � l  	 	����~��  �  �~   �  � � � W   	 " � � � k     � �  � � � s     � � � o    �}�} 0 currentdate currentDate � l      ��|�{ � n       � � �  ;     � o    �z�z 0 datelist dateList�|  �{   �  ��y � r     � � � [     � � � o    �x�x 0 currentdate currentDate � ]     � � � m    �w�w  � 1    �v
�v 
days � o      �u�u 0 currentdate currentDate�y   � ?     � � � o    �t�t 0 currentdate currentDate � o    �s�s 0 enddate endDate �  � � � l  # #�r�q�p�r  �q  �p   �  ��o � L   # % � � o   # $�n�n 0 datelist dateList�o   �  � � � l     �m�l�k�m  �l  �k   �  � � � l     �j�i�h�j  �i  �h   �  � � � l     �g�f�e�g  �f  �e   �  � � � l     �d � ��d   �   Main script execution    � � � � ,   M a i n   s c r i p t   e x e c u t i o n �  � � � l    i ��c�b � O     i � � � k    h � �    I   �a�`
�a .ascrcmnt****      � **** b     m     � " A n d   a w a y   w e   g o . . . l   �_�^ c    	 l   

�]�\
 I   
�[�Z�Y
�[ .misccurdldt    ��� null�Z  �Y  �]  �\  	 m   
 �X
�X 
TEXT�_  �^  �`    l   �W�V�U�W  �V  �U    r     n    I    �T�S�T 0 promptfordate promptForDate �R m     � @ E n t e r   t h e   s t a r t   d a t e   ( M M D D Y Y Y Y ) :�R  �S    f     o      �Q�Q 0 	startdate 	startDate  r    # n   ! I    !�P�O�P 0 promptfordate promptForDate �N m     �   < E n t e r   t h e   e n d   d a t e   ( M M D D Y Y Y Y ) :�N  �O    f     o      �M�M 0 enddate endDate !"! l  $ $�L�K�J�L  �K  �J  " #$# r   $ -%&% n  $ +'(' I   % +�I)�H�I 0 generatedates generateDates) *+* o   % &�G�G 0 	startdate 	startDate+ ,�F, o   & '�E�E 0 enddate endDate�F  �H  (  f   $ %& o      �D�D 0 alldates allDates$ -.- l  . .�C�B�A�C  �B  �A  . /0/ l  . .�@12�@  1 ; 5 Check on the number of dates we are about to process   2 �33 j   C h e c k   o n   t h e   n u m b e r   o f   d a t e s   w e   a r e   a b o u t   t o   p r o c e s s0 454 r   . f676 n   . b898 1   ^ b�?
�? 
bhit9 l  . ^:�>�=: I  . ^�<;<
�< .sysodlogaskr        TEXT; b   . I=>= b   . @?@? b   . <ABA b   . 7CDC b   . 5EFE m   . /GG �HH   C o u n t   o f   d a t e s :  F l  / 4I�;�:I I  / 4�9J�8
�9 .corecnte****       ****J o   / 0�7�7 0 alldates allDates�8  �;  �:  D m   5 6KK �LL    f r o m  B l  7 ;M�6�5M n   7 ;NON 4   8 ;�4P
�4 
cobjP m   9 :�3�3 O o   7 8�2�2 0 alldates allDates�6  �5  @ m   < ?QQ �RR    t o  > l  @ HS�1�0S n   @ HTUT 4   A H�/V
�/ 
cobjV l  B GW�.�-W I  B G�,X�+
�, .corecnte****       ****X o   B C�*�* 0 alldates allDates�+  �.  �-  U o   @ A�)�) 0 alldates allDates�1  �0  < �(YZ
�( 
btnsY J   L T[[ \]\ m   L O^^ �__  C a n c e l] `�'` m   O Raa �bb  O K�'  Z �&c�%
�& 
dfltc m   W X�$�$ �%  �>  �=  7 o      �#�# 0 userresponse userResponse5 d�"d l  g g�!� ��!  �   �  �"   � m     ee�                                                                                  sevs  alis    \  Macintosh HD               �~iBD ����System Events.app                                              �����~i        ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p    M a c i n t o s h   H D  -System/Library/CoreServices/System Events.app   / ��  �c  �b   � fgf l  jGh��h X   jGi�ji k   |Bkk lml l  | |�no�  n   log "in loop " & aDate   o �pp .   l o g   " i n   l o o p   "   &   a D a t em qrq O   |!sts k   � uu vwv r   � �xyx b   � �z{z b   � �|}| n  � �~~ I   � ����� 0 zeropad zeroPad� ��� c   � ���� n   � ���� 1   � ��
� 
day � o   � ��� 0 adate aDate� m   � ��
� 
nmbr�  �    f   � �} n  � ���� I   � ����� 0 zeropad zeroPad� ��� c   � ���� n   � ���� m   � ��
� 
mnth� o   � ��� 0 adate aDate� m   � ��
� 
nmbr�  �  �  f   � �{ l  � ����� c   � ���� n   � ���� 1   � ��
� 
year� o   � ��� 0 adate aDate� m   � ��

�
 
nmbr�  �  y o      �	�	 $0 formatteddatedmy formattedDateDMYw ��� r   � ���� b   � ���� b   � ���� b   � ���� b   � ���� l  � ����� c   � ���� n   � ���� 1   � ��
� 
year� o   � ��� 0 adate aDate� m   � ��
� 
nmbr�  �  � m   � ��� ���  -� n  � ���� I   � ����� 0 zeropad zeroPad� ��� c   � ���� n   � ���� m   � �� 
�  
mnth� o   � ����� 0 adate aDate� m   � ���
�� 
nmbr�  �  �  f   � �� m   � ��� ���  -� n  � ���� I   � �������� 0 zeropad zeroPad� ���� c   � ���� n   � ���� 1   � ���
�� 
day � o   � ����� 0 adate aDate� m   � ���
�� 
nmbr��  ��  �  f   � �� o      ���� $0 formatteddateymd formattedDateYMD� ��� l  � �������  � 4 . log "formattedDateDMY is " & formattedDateDMY   � ��� \   l o g   " f o r m a t t e d D a t e D M Y   i s   "   &   f o r m a t t e d D a t e D M Y� ��� l  � �������  � 4 . log "formattedDateYMD is " & formattedDateYMD   � ��� \   l o g   " f o r m a t t e d D a t e Y M D   i s   "   &   f o r m a t t e d D a t e Y M D� ��� l  � ���������  ��  ��  � ��� l  � �������  � 4 . for debugging, verify before opening web page   � ��� \   f o r   d e b u g g i n g ,   v e r i f y   b e f o r e   o p e n i n g   w e b   p a g e� ��� l  � �������  � � � set userResponse to button returned of (display dialog "Verify date DMY: " & formattedDateDMY & "Verify date YMD: " & formattedDateYMD buttons {"Cancel", "OK"} default button 2)   � ���d   s e t   u s e r R e s p o n s e   t o   b u t t o n   r e t u r n e d   o f   ( d i s p l a y   d i a l o g   " V e r i f y   d a t e   D M Y :   "   &   f o r m a t t e d D a t e D M Y   &   " V e r i f y   d a t e   Y M D :   "   &   f o r m a t t e d D a t e Y M D   b u t t o n s   { " C a n c e l " ,   " O K " }   d e f a u l t   b u t t o n   2 )� ��� l  � ���������  ��  ��  � ��� r   � ���� b   � ���� m   � ��� ��� � h t t p s : / / w w w . k m u . g o v . u a / e n / n e w s / z a h a l n i - b o i o v i - v t r a t y - p r o t y v n y k a - z - 2 4 0 2 2 0 2 2 - p o -� o   � ����� $0 formatteddatedmy formattedDateDMY� o      ���� 0 theurl theURL� ��� l  � ���������  ��  ��  � ��� I  � ������
�� .ascrcmnt****      � ****� b   � ���� m   � ��� ��� 
 U R L :  � o   � ����� 0 theurl theURL��  � ��� l  � ���������  ��  ��  � ��� I  � �������
�� .miscactvnull��� ��� null��  ��  � ��� I  �����
�� .GURLGURLnull��� ��� TEXT� o   ���� 0 theurl theURL��  � ��� l ���� I �����
�� .sysodelanull��� ��� nmbr� m  ���� ��  � $  Adjust the delay as necessary   � ��� <   A d j u s t   t h e   d e l a y   a s   n e c e s s a r y� ��� l ������  � &   Get the source code from Safari   � ��� @   G e t   t h e   s o u r c e   c o d e   f r o m   S a f a r i� ��� r  ��� n  ��� 1  ��
�� 
conT� 4  ���
�� 
docu� m  ���� � o      ���� 0 	thesource 	theSource� ���� l ������  � 8 2 log "after set theSource to source of document 1"   � ��� d   l o g   " a f t e r   s e t   t h e S o u r c e   t o   s o u r c e   o f   d o c u m e n t   1 "��  t m   | ���                                                                                  sfri  alis    p  Preboot                    ܰu;BD ����
Safari.app                                                     �����}��        ����  
 cu             Applications  F/:System:Volumes:Preboot:Cryptexes:App:System:Applications:Safari.app/   
 S a f a r i . a p p    P r e b o o t  -/Cryptexes/App/System/Applications/Safari.app   /System/Volumes/Preboot ��  r    l ""����   7 1 Assuming theSource contains the HTML source code    � b   A s s u m i n g   t h e S o u r c e   c o n t a i n s   t h e   H T M L   s o u r c e   c o d e  l ""��������  ��  ��    l ""��	
��  	   String to search for   
 � *   S t r i n g   t o   s e a r c h   f o r  r  ") m  "% �  > 4 0 4 < / d i v > o      ���� 0 searchstring searchString  l **��������  ��  ��    l **����   U O Use AppleScript's text item delimiters to check if the string is in the source    � �   U s e   A p p l e S c r i p t ' s   t e x t   i t e m   d e l i m i t e r s   t o   c h e c k   i f   t h e   s t r i n g   i s   i n   t h e   s o u r c e  r  *5 o  *-���� 0 searchstring searchString n      1  04��
�� 
txdl 1  -0��
�� 
ascr   r  6A!"! n  6=#$# 2 9=��
�� 
citm$ o  69���� 0 	thesource 	theSource" o      ���� 0 	textitems 	textItems  %&% l BM'()' r  BM*+* m  BE,, �--  + n     ./. 1  HL��
�� 
txdl/ 1  EH��
�� 
ascr(   Reset the delimiters   ) �00 *   R e s e t   t h e   d e l i m i t e r s& 121 l NN��������  ��  ��  2 343 l NN��56��  5 9 3 Check if the string indicating 404 error was found   6 �77 f   C h e c k   i f   t h e   s t r i n g   i n d i c a t i n g   4 0 4   e r r o r   w a s   f o u n d4 898 Z  N�:;��<: ?  NW=>= l NU?����? I NU��@��
�� .corecnte****       ****@ o  NQ���� 0 	textitems 	textItems��  ��  ��  > m  UV���� ; k  ZgAA BCB I Ze��D��
�� .ascrcmnt****      � ****D b  ZaEFE m  Z]GG �HH  4 0 4   N O T   F O U N D :F o  ]`���� 0 theurl theURL��  C I��I l ff��JK��  J R L Handle the case where the string is found, i.e., don't save the page source   K �LL �   H a n d l e   t h e   c a s e   w h e r e   t h e   s t r i n g   i s   f o u n d ,   i . e . ,   d o n ' t   s a v e   t h e   p a g e   s o u r c e��  ��  < k  j�MM NON l jj��PQ��  P 4 . Handle the case where the string is not found   Q �RR \   H a n d l e   t h e   c a s e   w h e r e   t h e   s t r i n g   i s   n o t   f o u n dO STS l jj��UV��  U $  Define the file path and name   V �WW <   D e f i n e   t h e   f i l e   p a t h   a n d   n a m eT XYX r  jyZ[Z I ju��\]
�� .earsffdralis        afdr\ m  jm��
�� afdrcusr] ��^��
�� 
rtyp^ m  pq��
�� 
TEXT��  [ o      ���� 0 homepath homePathY _`_ r  z�aba b  z�cdc o  z}���� 0 homepath homePathd m  }�ee �ff b D o c u m e n t s : R _ l o c a l _ r e p o s : u k r a i n e s t a t s : u k r _ r e p o r t s :b o      ���� 0 
folderpath 
folderPath` ghg r  ��iji b  ��klk b  ��mnm m  ��oo �pp  u k r a i n e _ s t a t s _n o  ������ $0 formatteddateymd formattedDateYMDl m  ��qq �rr 
 . h t m lj o      ���� 0 filename fileNameh sts r  ��uvu b  ��wxw o  ������ 0 
folderpath 
folderPathx o  ������ 0 filename fileNamev o      ���� 0 fullfilepath fullFilePatht yzy l ����������  ��  ��  z {|{ l ����}~��  } , & log "fullFilePath is " & fullFilePath   ~ � L   l o g   " f u l l F i l e P a t h   i s   "   &   f u l l F i l e P a t h| ��� l ����������  ��  ��  � ��� l ��������  � 4 . Replace colons with slashes for shell command   � ��� \   R e p l a c e   c o l o n s   w i t h   s l a s h e s   f o r   s h e l l   c o m m a n d� ��� r  ����� n  ����� 1  ����
�� 
psxp� o  ������ 0 fullfilepath fullFilePath� o      ���� 0 	posixpath 	posixPath� ��� l ����������  ��  ��  � ��� I �������
�� .ascrcmnt****      � ****� o  ������ 0 	posixpath 	posixPath��  � ��� l ����������  ��  ��  � ��� l ��������  �   Save the source code   � ��� *   S a v e   t h e   s o u r c e   c o d e� ���� I �������
�� .sysoexecTEXT���     TEXT� b  ����� b  ����� b  ����� b  ����� b  ����� m  ���� ���  m k d i r   - p  � n  ����� 1  ����
�� 
strq� l �������� n  ����� 1  ����
�� 
psxp� o  ������ 0 
folderpath 
folderPath��  ��  � m  ���� ���  ;   e c h o  � n  ����� 1  ����
�� 
strq� o  ������ 0 	thesource 	theSource� m  ���� ���    >  � n  ����� 1  ����
�� 
strq� o  ������ 0 	posixpath 	posixPath��  ��  9 ��� l ����~�}�  �~  �}  � ��� O  �@��� k  �?�� ��� l ���|���|  � 1 + Check if there is at least one window open   � ��� V   C h e c k   i f   t h e r e   i s   a t   l e a s t   o n e   w i n d o w   o p e n� ��{� Z  �?���z�y� ?  ����� l ����x�w� I ���v��u
�v .corecnte****       ****� 2 ���t
�t 
cwin�u  �x  �w  � m  ���s�s  � k  �;�� ��� r  � ��� 4 ���r�
�r 
cwin� m  ���q�q � o      �p�p 0 currentwindow currentWindow� ��� l �o�n�m�o  �n  �m  � ��� l �l���l  � D > Check if there is at least one tab open in the current window   � ��� |   C h e c k   i f   t h e r e   i s   a t   l e a s t   o n e   t a b   o p e n   i n   t h e   c u r r e n t   w i n d o w� ��k� Z  ;���j�i� ?  ��� l ��h�g� I �f��e
�f .corecnte****       ****� n  ��� 2 �d
�d 
bTab� o  �c�c 0 currentwindow currentWindow�e  �h  �g  � m  �b�b  � k  7�� ��� r  ��� n  ��� 1  �a
�a 
cTab� o  �`�` 0 currentwindow currentWindow� o      �_�_ 0 
currenttab 
currentTab� ��� r  (��� n  $��� 1   $�^
�^ 
pnam� o   �]�] 0 
currenttab 
currentTab� o      �\�\ 0 tabname tabName� ��� l ))�[�Z�Y�[  �Z  �Y  � ��� l ))�X���X  � &   Log the name of the current tab   � ��� @   L o g   t h e   n a m e   o f   t h e   c u r r e n t   t a b� ��� l ))�W���W  � $  log "Closing tab: " & tabName   � ��� <   l o g   " C l o s i n g   t a b :   "   &   t a b N a m e� ��� l ))�V�U�T�V  �U  �T  � ��� l ))�S���S  �   Close the current tab   � ��� ,   C l o s e   t h e   c u r r e n t   t a b� ��R� O )7��� I /6�Q �P
�Q .coreclosnull���     obj   o  /2�O�O 0 
currenttab 
currentTab�P  � o  ),�N�N 0 currentwindow currentWindow�R  �j  �i  �k  �z  �y  �{  � m  ���                                                                                  sfri  alis    p  Preboot                    ܰu;BD ����
Safari.app                                                     �����}��        ����  
 cu             Applications  F/:System:Volumes:Preboot:Cryptexes:App:System:Applications:Safari.app/   
 S a f a r i . a p p    P r e b o o t  -/Cryptexes/App/System/Applications/Safari.app   /System/Volumes/Preboot ��  � �M l AA�L�K�J�L  �K  �J  �M  � 0 adate aDatej o   m n�I�I 0 alldates allDates�  �  g  l     �H�G�F�H  �G  �F    l     �E�E   = 7 Function to add a leading zero to single-digit numbers    �		 n   F u n c t i o n   t o   a d d   a   l e a d i n g   z e r o   t o   s i n g l e - d i g i t   n u m b e r s 

 i     I      �D�C�D 0 zeropad zeroPad �B o      �A�A 0 anumber  �B  �C   Z     �@ A      o     �?�? 0 anumber   m    �>�> 
 L     b     m     �  0 l   
�=�< c    
 o    �;�; 0 anumber   m    	�:
�: 
TEXT�=  �<  �@   L     c     o    �9�9 0 anumber   m    �8
�8 
TEXT  �7  l     �6�5�4�6  �5  �4  �7       �3!"#$%�3  ! �2�1�0�/�2 0 promptfordate promptForDate�1 0 generatedates generateDates�0 0 zeropad zeroPad
�/ .aevtoappnull  �   � ****" �. �-�,&'�+�. 0 promptfordate promptForDate�- �*(�* (  �)�) 0 promptmessage promptMessage�,  & �(�'�&�%�$�#�"�!� ���( 0 promptmessage promptMessage�' 0 	firstdate 	firstDate�& 0 
currentday 
currentDay�% 0 currentmonth currentMonth�$ 0 currentyear currentYear�# 0 defaultdate defaultDate�" 0 	inputdate 	inputDate�! 0 daypart dayPart�  0 	monthpart 	monthPart� 0 yearpart yearPart� *0 formatteddatestring formattedDateString' �������������� w� }�����
 � ��	
� .misccurdldt    ��� null� 
� 
days
� 
day 
� 
nmbr
� 
mnth
� 
year� 0 zeropad zeroPad
� 
TEXT
� 
dtxt
� .sysodlogaskr        TEXT
� 
ttxt
� 
leng� 
� 
btns
� 
dflt� � 0 promptfordate promptForDate
� 
ctxt�
 
�	 
ldt �+ �*j  �� E�O��,�&E�O��,�&E�O��,�&E�O)�k+ )�k+ %�%�&E�O��l 
�,E�O��,� ��a kva ka  
O*�k+ Y hO�[a \[Zk\Zl2E�O�[a \[Zm\Za 2E�O�[a \[Za \Z�2E�O�a %�%a %�%E�O*a �/E# � ���)*�� 0 generatedates generateDates� �+� +  ��� 0 	startdate 	startDate� 0 enddate endDate�  ) �� ����� 0 	startdate 	startDate�  0 enddate endDate�� 0 currentdate currentDate�� 0 datelist dateList* ��
�� 
days� &�E�OjvE�O h����6GO�k� E�[OY��O�$ ������,-���� 0 zeropad zeroPad�� ��.�� .  ���� 0 anumber  ��  , ���� 0 anumber  - ������ 

�� 
TEXT�� �� ��&%Y ��&% ��/����01��
�� .aevtoappnull  �   � ****/ k    G22  �33 f����  ��  ��  0 ���� 0 adate aDate1 Oe����������������G��K��Q��^a���������������������������������������������������������,G��������e��oq�������������������������������
�� .misccurdldt    ��� null
�� 
TEXT
�� .ascrcmnt****      � ****�� 0 promptfordate promptForDate�� 0 	startdate 	startDate�� 0 enddate endDate�� 0 generatedates generateDates�� 0 alldates allDates
�� .corecnte****       ****
�� 
cobj
�� 
btns
�� 
dflt�� 
�� .sysodlogaskr        TEXT
�� 
bhit�� 0 userresponse userResponse
�� 
kocl
�� 
day 
�� 
nmbr�� 0 zeropad zeroPad
�� 
mnth
�� 
year�� $0 formatteddatedmy formattedDateDMY�� $0 formatteddateymd formattedDateYMD�� 0 theurl theURL
�� .miscactvnull��� ��� null
�� .GURLGURLnull��� ��� TEXT�� 
�� .sysodelanull��� ��� nmbr
�� 
docu
�� 
conT�� 0 	thesource 	theSource�� 0 searchstring searchString
�� 
ascr
�� 
txdl
�� 
citm�� 0 	textitems 	textItems
�� afdrcusr
�� 
rtyp
�� .earsffdralis        afdr�� 0 homepath homePath�� 0 
folderpath 
folderPath�� 0 filename fileName�� 0 fullfilepath fullFilePath
�� 
psxp�� 0 	posixpath 	posixPath
�� 
strq
�� .sysoexecTEXT���     TEXT
�� 
cwin�� 0 currentwindow currentWindow
�� 
bTab
�� 
cTab�� 0 
currenttab 
currentTab
�� 
pnam�� 0 tabname tabName
�� .coreclosnull���     obj ��H� f�*j �&%j O)�k+ E�O)�k+ E�O)��l+ 
E�O��j %�%��k/%a %���j /%a a a lva la  a ,E` OPUO��[a �l kh  a  �)�a ,a &k+ )�a ,a &k+ %�a ,a &%E`  O�a ,a &a !%)�a ,a &k+ %a "%)�a ,a &k+ %E` #Oa $_  %E` %Oa &_ %%j O*j 'O_ %j (Oa )j *O*a +k/a ,,E` -OPUOa .E` /O_ /_ 0a 1,FO_ -a 2-E` 3Oa 4_ 0a 1,FO_ 3j k a 5_ %%j OPY ya 6a 7�l 8E` 9O_ 9a :%E` ;Oa <_ #%a =%E` >O_ ;_ >%E` ?O_ ?a @,E` AO_ Aj Oa B_ ;a @,a C,%a D%_ -a C,%a E%_ Aa C,%j FOa  Y*a G-j j J*a Gk/E` HO_ Ha I-j j +_ Ha J,E` KO_ Ka L,E` MO_ H 	_ Kj NUY hY hUOP[OY�4ascr  ��ޭ