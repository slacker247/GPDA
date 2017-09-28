/*
 File:		CountryField.java
 CVS Info:      $Id$
 Compiler:	jdk 1.2.2
 */

package disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * Country Field -- This field shall specify the country to which the design of the entity is attributed. This field shall be represented by a 16-bit enumeration. Values for this field are defined in Section 4 in EBV-DOC.
 *@version 1.1
 *@author Ronan Fauglas
 *@author <a href="mailto:brutzman@nps.navy.mil?subject=dis-java-vrml: disEnumerations.CountryField feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/d.htm">Country Field</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/pdu/d.htm">Country Field</A> (SISO)
 *<dd>          JDBE:<a href="http://208.145.129.4/jdbe/proj/dis_cd/dis-dd/">DIS Data Dictionary Version 1.0a (DIS-DD)</A>
 *<dd>		Perl script (converting html enumerations to java enumerations)
 *		<A href="../../../../../../mil/navy/nps/disEnumerations/convertJdbeDisEnumerationsToJava.pl"><i>convertJdbeDisEnumerationsToJava.pl</i></A> (local) or
 *		<A href="http://web.3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/convertJdbeDisEnumerationsToJava.pl">
 *	 	      <i>http://web.3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/convertJdbeDisEnumerationsToJava.pl</i></A>
 *<dd>		"Named Constants," <i>The Java Programming Language</i>, Gosling & Arnold.
 *
 *<dt><b>Explanation:</b>
 *<dd>This file has been automatically generated from a local copy of the
 *  <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/dis-dd.html">DIS Data Dictionary</A> at
 *  <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/">http://SISO.sc.ist.ucf.edu/dis/dis-dd/</A>
 *  html source file by
 *  <A href="../../../../../../mil/navy/nps/disEnumerations/convertJdbeDisEnumerationsToJava.pl">convertJdbeDisEnumerationsToJava.pl</a> (local) or
 *  <A href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/convertJdbeDisEnumerationsToJava.pl">http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/convertJdbeDisEnumerationsToJava.pl</a>.
 *  <P>
 *  This is effectively a C-style enumeration. Java doesn't do enumerations
 *  like C, so you have to wrap a class around it. It's a bit more typing,
 *  but pretty simple-minded. 
 *  Note that the variables are declared public. The default for access 
 *  is package-wide, but these variables might need to be accessed from
 *  outside the package. Since all the variables are final (i.e. constant), nobody can
 *  change anything anyway, so this is no biggie.<p>
 *  To use these enumerations in your Java code, import the package first:
 *         <b><PRE>import mil.navy.nps.disEnumerations.*;</PRE></b>
 *  You access this via something like <b><code>CountryField.INDIA</code></b>, i.e. combine
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/CountryField.java"><i>CountryField.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/CountryField.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/CountryField.java</i></a>
 *
 */
public class CountryField_1 extends Object
{
/**
 *(0) Other
 */
public static final short OTHER = 0;

/**
 *(1) Afghanistan
 */
public static final short AFGHANISTAN = 1;

/**
 *(10) Argentina
 */
public static final short ARGENTINA = 10;

/**
 *(100) Indonesia
 */
public static final short INDONESIA = 100;

/**
 *(101) Iran
 */
public static final short IRAN = 101;

/**
 *(102) Iraq
 */
public static final short IRAQ = 102;

/**
 *(104) Ireland
 */
public static final short IRELAND = 104;

/**
 *(105) Israel
 */
public static final short ISRAEL = 105;

/**
 *(106) Italy
 */
public static final short ITALY = 106;

/**
 *(107) Cote D'Ivoire (aka Ivory Coast)
 */
public static final short COTEDIVOIRE = 107;

/**
 *(108) Jamaica
 */
public static final short JAMAICA = 108;

/**
 *(109) Jan Mayen (Norway)
 */
public static final short JANMAYEN = 109;

/**
 *(11) Aruba
 */
public static final short ARUBA = 11;

/**
 *(110) Japan
 */
public static final short JAPAN = 110;

/**
 *(111) Jarvis Island (United States)
 */
public static final short JARVISISLAND = 111;

/**
 *(112) Jersey (United Kingdom)
 */
public static final short JERSEY = 112;

/**
 *(113) Johnston Atoll (United States)
 */
public static final short JOHNSTONATOLL = 113;

/**
 *(114) Jordan
 */
public static final short JORDAN = 114;

/**
 *(115) Juan  de Nova Island
 */
public static final short JUANDENOVAISLAND = 115;

/**
 *(116) Kenya
 */
public static final short KENYA = 116;

/**
 *(117) Kingman Reef  (United States)
 */
public static final short KINGMANREEF = 117;

/**
 *(118) Kiribati
 */
public static final short KIRIBATI = 118;

/**
 *(119) Korea, Democratic People's Republic of (North)
 */
public static final short KOREADEMOCRATICPEOPLESREPUBLICOF = 119;

/**
 *(12) Ashmore and Cartier Islands (Australia)
 */
public static final short ASHMOREANDCARTIERISLANDS = 12;

/**
 *(120) Korea, Republic of (South)
 */
public static final short KOREAREPUBLICOF = 120;

/**
 *(121) Kuwait
 */
public static final short KUWAIT = 121;

/**
 *(122) Laos
 */
public static final short LAOS = 122;

/**
 *(123) Lebanon
 */
public static final short LEBANON = 123;

/**
 *(124) Lesotho
 */
public static final short LESOTHO = 124;

/**
 *(125) Liberia
 */
public static final short LIBERIA = 125;

/**
 *(126) Libya
 */
public static final short LIBYA = 126;

/**
 *(127) Liechtenstein
 */
public static final short LIECHTENSTEIN = 127;

/**
 *(128) Luxembourg
 */
public static final short LUXEMBOURG = 128;

/**
 *(129) Madagascar
 */
public static final short MADAGASCAR = 129;

/**
 *(13) Australia
 */
public static final short AUSTRALIA = 13;

/**
 *(130) Macau (Portugal)
 */
public static final short MACAU = 130;

/**
 *(131) Malawi
 */
public static final short MALAWI = 131;

/**
 *(132) Malaysia
 */
public static final short MALAYSIA = 132;

/**
 *(133) Maldives
 */
public static final short MALDIVES = 133;

/**
 *(134) Mali
 */
public static final short MALI = 134;

/**
 *(135) Malta
 */
public static final short MALTA = 135;

/**
 *(136) Man, Isle of (United Kingdom)
 */
public static final short MANISLEOF = 136;

/**
 *(137) Marshall Islands
 */
public static final short MARSHALLISLANDS = 137;

/**
 *(138) Martinique  (France)
 */
public static final short MARTINIQUE = 138;

/**
 *(139) Mauritania
 */
public static final short MAURITANIA = 139;

/**
 *(14) Austria
 */
public static final short AUSTRIA = 14;

/**
 *(140) Mauritius
 */
public static final short MAURITIUS = 140;

/**
 *(141) Mayotte (France)
 */
public static final short MAYOTTE = 141;

/**
 *(142) Mexico
 */
public static final short MEXICO = 142;

/**
 *(143) Micronesia, Federative States of
 */
public static final short MICRONESIAFEDERATIVESTATESOF = 143;

/**
 *(144) Monaco
 */
public static final short MONACO = 144;

/**
 *(145) Mongolia
 */
public static final short MONGOLIA = 145;

/**
 *(146) Montserrat (United Kingdom)
 */
public static final short MONTSERRAT = 146;

/**
 *(147) Morocco
 */
public static final short MOROCCO = 147;

/**
 *(148) Mozambique
 */
public static final short MOZAMBIQUE = 148;

/**
 *(149) Namibia (South West Africa)
 */
public static final short NAMIBIA = 149;

/**
 *(15) Bahamas
 */
public static final short BAHAMAS = 15;

/**
 *(150) Nauru
 */
public static final short NAURU = 150;

/**
 *(151) Navassa Island (United States)
 */
public static final short NAVASSAISLAND = 151;

/**
 *(152) Nepal
 */
public static final short NEPAL = 152;

/**
 *(153) Netherlands
 */
public static final short NETHERLANDS = 153;

/**
 *(154) Netherlands Antilles (Curacao, Bonaire, Saba, Sint Maarten Sint Eustatius)
 */
public static final short NETHERLANDSANTILLES = 154;

/**
 *(155) New Caledonia (France)
 */
public static final short NEWCALEDONIA = 155;

/**
 *(156) New Zealand
 */
public static final short NEWZEALAND = 156;

/**
 *(157) Nicaragua
 */
public static final short NICARAGUA = 157;

/**
 *(158) Niger
 */
public static final short NIGER = 158;

/**
 *(159) Nigeria
 */
public static final short NIGERIA = 159;

/**
 *(16) Bahrain
 */
public static final short BAHRAIN = 16;

/**
 *(160) Niue (New Zealand)
 */
public static final short NIUE = 160;

/**
 *(161) Norfolk Island (Australia)
 */
public static final short NORFOLKISLAND = 161;

/**
 *(162) Northern Mariana Islands (United States)
 */
public static final short NORTHERNMARIANAISLANDS = 162;

/**
 *(163) Norway
 */
public static final short NORWAY = 163;

/**
 *(164) Oman
 */
public static final short OMAN = 164;

/**
 *(165) Pakistan
 */
public static final short PAKISTAN = 165;

/**
 *(166) Palmyra Atoll (United States)
 */
public static final short PALMYRAATOLL = 166;

/**
 *(168) Panama
 */
public static final short PANAMA = 168;

/**
 *(169) Papua New Guinea
 */
public static final short PAPUANEWGUINEA = 169;

/**
 *(17) Baker Island (United States)
 */
public static final short BAKERISLAND = 17;

/**
 *(170) Paracel Islands (International - Occupied by China, also claimed by Taiwan and Vietnam)
 */
public static final short PARACELISLANDS = 170;

/**
 *(171) Paraguay
 */
public static final short PARAGUAY = 171;

/**
 *(172) Peru
 */
public static final short PERU = 172;

/**
 *(173) Philippines
 */
public static final short PHILIPPINES = 173;

/**
 *(174) Pitcairn Islands (United Kingdom)
 */
public static final short PITCAIRNISLANDS = 174;

/**
 *(175) Poland
 */
public static final short POLAND = 175;

/**
 *(176) Portugal
 */
public static final short PORTUGAL = 176;

/**
 *(177) Puerto Rico (United States)
 */
public static final short PUERTORICO = 177;

/**
 *(178) Qatar
 */
public static final short QATAR = 178;

/**
 *(179) Reunion (France)
 */
public static final short REUNION = 179;

/**
 *(18) Bangladesh
 */
public static final short BANGLADESH = 18;

/**
 *(180) Romania
 */
public static final short ROMANIA = 180;

/**
 *(181) Rwanda
 */
public static final short RWANDA = 181;

/**
 *(182) St Kitts and Nevis
 */
public static final short STKITTSANDNEVIS = 182;

/**
 *(183) St Helena (United Kingdom)
 */
public static final short STHELENA = 183;

/**
 *(184) St Lucia
 */
public static final short STLUCIA = 184;

/**
 *(185) St Pierre and Miquelon (France)
 */
public static final short STPIERREANDMIQUELON = 185;

/**
 *(186) St Vincent and the Grenadines
 */
public static final short STVINCENTANDTHEGRENADINES = 186;

/**
 *(187) San Marino
 */
public static final short SANMARINO = 187;

/**
 *(188) Sao Tome and Principe
 */
public static final short SAOTOMEANDPRINCIPE = 188;

/**
 *(189) Saudi Arabia
 */
public static final short SAUDIARABIA = 189;

/**
 *(19) Barbados
 */
public static final short BARBADOS = 19;

/**
 *(190) Senegal
 */
public static final short SENEGAL = 190;

/**
 *(191) Seychelles
 */
public static final short SEYCHELLES = 191;

/**
 *(192) Sierra Leone
 */
public static final short SIERRALEONE = 192;

/**
 *(193) Singapore
 */
public static final short SINGAPORE = 193;

/**
 *(194) Solomon Islands
 */
public static final short SOLOMONISLANDS = 194;

/**
 *(195) Somalia
 */
public static final short SOMALIA = 195;

/**
 *(196) South Georgia and the South Sandwich Islands(United Kingdom)
 */
public static final short SOUTHGEORGIAANDTHESOUTHSANDWICHISLANDS = 196;

/**
 *(197) South Africa
 */
public static final short SOUTHAFRICA = 197;

/**
 *(198) Spain
 */
public static final short SPAIN = 198;

/**
 *(199) Spratly Islands (International - parts occupied and claimed by China,Malaysia, Philippines, Taiwan, Vietnam)
 */
public static final short SPRATLYISLANDS = 199;

/**
 *(2) Albania
 */
public static final short ALBANIA = 2;

/**
 *(20) Bassas  da India (France)
 */
public static final short BASSASDAINDIA = 20;

/**
 *(200) Sri Lanka
 */
public static final short SRILANKA = 200;

/**
 *(201) Sudan
 */
public static final short SUDAN = 201;

/**
 *(202) Suriname
 */
public static final short SURINAME = 202;

/**
 *(203) Svalbard (Norway)
 */
public static final short SVALBARD = 203;

/**
 *(204) Swaziland
 */
public static final short SWAZILAND = 204;

/**
 *(205) Sweden
 */
public static final short SWEDEN = 205;

/**
 *(206) Switzerland
 */
public static final short SWITZERLAND = 206;

/**
 *(207) Syria
 */
public static final short SYRIA = 207;

/**
 *(208) Taiwan
 */
public static final short TAIWAN = 208;

/**
 *(209) Tanzania
 */
public static final short TANZANIA = 209;

/**
 *(21) Belgium
 */
public static final short BELGIUM = 21;

/**
 *(210) Thailand
 */
public static final short THAILAND = 210;

/**
 *(211) Togo
 */
public static final short TOGO = 211;

/**
 *(212) Tokelau (New Zealand)
 */
public static final short TOKELAU = 212;

/**
 *(213) Tonga
 */
public static final short TONGA = 213;

/**
 *(214) Trinidad and Tobago
 */
public static final short TRINIDADANDTOBAGO = 214;

/**
 *(215) Tromelin Island (France)
 */
public static final short TROMELINISLAND = 215;

/**
 *(216) Pacific Islands, Trust Territory of the (Palau)
 */
public static final short PACIFICISLANDSTRUSTTERRITORYOFTHE = 216;

/**
 *(217) Tunisia
 */
public static final short TUNISIA = 217;

/**
 *(218) Turkey
 */
public static final short TURKEY = 218;

/**
 *(219) Turks and Caicos Islands (United Kingdom)
 */
public static final short TURKSANDCAICOSISLANDS = 219;

/**
 *(22) Belize
 */
public static final short BELIZE = 22;

/**
 *(220) Tuvalu
 */
public static final short TUVALU = 220;

/**
 *(221) Uganda
 */
public static final short UGANDA = 221;

/**
 *(222) Commonwealth of Independent States
 */
public static final short COMMONWEALTHOFINDEPENDENTSTATES = 222;

/**
 *(223) United Arab Emirates
 */
public static final short UNITEDARABEMIRATES = 223;

/**
 *(224) United Kingdom
 */
public static final short UNITEDKINGDOM = 224;

/**
 *(225) United States
 */
public static final short UNITEDSTATES = 225;

/**
 *(226) Uruguay
 */
public static final short URUGUAY = 226;

/**
 *(227) Vanuatu
 */
public static final short VANUATU = 227;

/**
 *(228) Vatican City (Holy See)
 */
public static final short VATICANCITY = 228;

/**
 *(229) Venezuela
 */
public static final short VENEZUELA = 229;

/**
 *(23) Benin (aka Dahomey)
 */
public static final short BENIN = 23;

/**
 *(230) Vietnam
 */
public static final short VIETNAM = 230;

/**
 *(231) Virgin Islands (United States)
 */
public static final short VIRGINISLANDS = 231;

/**
 *(232) Wake Island (United States)
 */
public static final short WAKEISLAND = 232;

/**
 *(233) Wallis and Futuna (France)
 */
public static final short WALLISANDFUTUNA = 233;

/**
 *(234) Western Sahara
 */
public static final short WESTERNSAHARA = 234;

/**
 *(235) West Bank (Israel)
 */
public static final short WESTBANK = 235;

/**
 *(236) Western Samoa
 */
public static final short WESTERNSAMOA = 236;

/**
 *(237) Yemen
 */
public static final short YEMEN = 237;

/**
 *(24) Bermuda (United Kingdom)
 */
public static final short BERMUDA = 24;

/**
 *(241) Zaire
 */
public static final short ZAIRE = 241;

/**
 *(242) Zambia
 */
public static final short ZAMBIA = 242;

/**
 *(243) Zimbabwe
 */
public static final short ZIMBABWE = 243;

/**
 *(244) Armenia
 */
public static final short ARMENIA = 244;

/**
 *(245) Azerbaijan
 */
public static final short AZERBAIJAN = 245;

/**
 *(246) Belarus
 */
public static final short BELARUS = 246;

/**
 *(247) Bosnia and Hercegovina
 */
public static final short BOSNIAANDHERCEGOVINA = 247;

/**
 *(248) Clipperton Island (France)
 */
public static final short CLIPPERTONISLAND = 248;

/**
 *(249) Croatia
 */
public static final short CROATIA = 249;

/**
 *(25) Bhutan
 */
public static final short BHUTAN = 25;

/**
 *(250) Estonia
 */
public static final short ESTONIA = 250;

/**
 *(251) Georgia
 */
public static final short GEORGIA = 251;

/**
 *(252) Kazakhstan
 */
public static final short KAZAKHSTAN = 252;

/**
 *(253) Kyrgyzstan
 */
public static final short KYRGYZSTAN = 253;

/**
 *(254) Latvia
 */
public static final short LATVIA = 254;

/**
 *(255) Lithuania
 */
public static final short LITHUANIA = 255;

/**
 *(256) Macedonia
 */
public static final short MACEDONIA = 256;

/**
 *(257) Midway Islands (United States)
 */
public static final short MIDWAYISLANDS = 257;

/**
 *(258) Moldova
 */
public static final short MOLDOVA = 258;

/**
 *(259) Montenegro
 */
public static final short MONTENEGRO = 259;

/**
 *(26) Bolivia
 */
public static final short BOLIVIA = 26;

/**
 *(260) Russia
 */
public static final short RUSSIA = 260;

/**
 *(261) Serbia and Montenegro (Montenegro to separate)
 */
public static final short SERBIAANDMONTENEGRO = 261;

/**
 *(262) Slovenia
 */
public static final short SLOVENIA = 262;

/**
 *(263) Tajikistan
 */
public static final short TAJIKISTAN = 263;

/**
 *(264) Turkmenistan
 */
public static final short TURKMENISTAN = 264;

/**
 *(265) Ukraine
 */
public static final short UKRAINE = 265;

/**
 *(266) Uzbekistan
 */
public static final short UZBEKISTAN = 266;

/**
 *(27) Botswana
 */
public static final short BOTSWANA = 27;

/**
 *(28) Bouvet Island (Norway)
 */
public static final short BOUVETISLAND = 28;

/**
 *(29) Brazil
 */
public static final short BRAZIL = 29;

/**
 *(3) Algeria
 */
public static final short ALGERIA = 3;

/**
 *(30) British Indian Ocean Territory (United Kingdom)
 */
public static final short BRITISHINDIANOCEANTERRITORY = 30;

/**
 *(31) British Virgin Islands (United Kingdom)
 */
public static final short BRITISHVIRGINISLANDS = 31;

/**
 *(32) Brunei
 */
public static final short BRUNEI = 32;

/**
 *(33) Bulgaria
 */
public static final short BULGARIA = 33;

/**
 *(34) Burkina  (aka Burkina Faso or Upper Volta
 */
public static final short BURKINAAKABURKINAFASOORUPPERVOLTA = 34;

/**
 *(35) Burma (Myanmar)
 */
public static final short BURMA = 35;

/**
 *(36) Burundi
 */
public static final short BURUNDI = 36;

/**
 *(37) Cambodia (aka Kampuchea)
 */
public static final short CAMBODIA = 37;

/**
 *(38) Cameroon
 */
public static final short CAMEROON = 38;

/**
 *(39) Canada
 */
public static final short CANADA = 39;

/**
 *(4) American Samoa (United States)
 */
public static final short AMERICANSAMOA = 4;

/**
 *(40) Cape Verde, Republic of
 */
public static final short CAPEVERDEREPUBLICOF = 40;

/**
 *(41) Cayman Islands (United Kingdom)
 */
public static final short CAYMANISLANDS = 41;

/**
 *(42) Central African Republic
 */
public static final short CENTRALAFRICANREPUBLIC = 42;

/**
 *(43) Chad
 */
public static final short CHAD = 43;

/**
 *(44) Chile
 */
public static final short CHILE = 44;

/**
 *(45) China, People's Republic of
 */
public static final short CHINAPEOPLESREPUBLICOF = 45;

/**
 *(46) Christmas Island (Australia)
 */
public static final short CHRISTMASISLAND = 46;

/**
 *(47) Cocos (Keeling) Islands (Australia)
 */
public static final short COCOSKEELINGISLANDS = 47;

/**
 *(48) Colombia
 */
public static final short COLOMBIA = 48;

/**
 *(49) Comoros
 */
public static final short COMOROS = 49;

/**
 *(5) Andorra
 */
public static final short ANDORRA = 5;

/**
 *(50) Congo, Republic of
 */
public static final short CONGOREPUBLICOF = 50;

/**
 *(51) Cook Islands (New Zealand)
 */
public static final short COOKISLANDS = 51;

/**
 *(52) Coral Sea Islands (Australia)
 */
public static final short CORALSEAISLANDS = 52;

/**
 *(53) Costa Rica
 */
public static final short COSTARICA = 53;

/**
 *(54) Cuba
 */
public static final short CUBA = 54;

/**
 *(55) Cyprus
 */
public static final short CYPRUS = 55;

/**
 *(56) Czechoslovakia (separating into Czech Republic and Slovak Republic)
 */
public static final short CZECHOSLOVAKIA = 56;

/**
 *(57) Denmark
 */
public static final short DENMARK = 57;

/**
 *(58) Djibouti
 */
public static final short DJIBOUTI = 58;

/**
 *(59) Dominica
 */
public static final short DOMINICA = 59;

/**
 *(6) Angola
 */
public static final short ANGOLA = 6;

/**
 *(60) Dominican Republic
 */
public static final short DOMINICANREPUBLIC = 60;

/**
 *(61) Ecuador
 */
public static final short ECUADOR = 61;

/**
 *(62) Egypt
 */
public static final short EGYPT = 62;

/**
 *(63) El Salvador
 */
public static final short ELSALVADOR = 63;

/**
 *(64) Equatorial Guinea
 */
public static final short EQUATORIALGUINEA = 64;

/**
 *(65) Ethiopia
 */
public static final short ETHIOPIA = 65;

/**
 *(66) Europa Island (France)
 */
public static final short EUROPAISLAND = 66;

/**
 *(67) Falkland Islands (aka Islas Malvinas) (United Kingdom)
 */
public static final short FALKLANDISLANDSAKAISLASMALVINAS = 67;

/**
 *(68) Faroe Islands (Denmark)
 */
public static final short FAROEISLANDS = 68;

/**
 *(69) Fiji
 */
public static final short FIJI = 69;

/**
 *(7) Anguilla
 */
public static final short ANGUILLA = 7;

/**
 *(70) Finland
 */
public static final short FINLAND = 70;

/**
 *(71) France
 */
public static final short FRANCE = 71;

/**
 *(72) French Guiana (France)
 */
public static final short FRENCHGUIANA = 72;

/**
 *(73) French Polynesia (France)
 */
public static final short FRENCHPOLYNESIA = 73;

/**
 *(74) French Southern and Antarctic Islands (France)
 */
public static final short FRENCHSOUTHERNANDANTARCTICISLANDS = 74;

/**
 *(75) Gabon
 */
public static final short GABON = 75;

/**
 *(76) Gambia, The
 */
public static final short GAMBIATHE = 76;

/**
 *(77) Gaza Strip (Israel)
 */
public static final short GAZASTRIP = 77;

/**
 *(78) Germany
 */
public static final short GERMANY = 78;

/**
 *(79) Ghana
 */
public static final short GHANA = 79;

/**
 *(8) Antarctica (International)
 */
public static final short ANTARCTICA = 8;

/**
 *(80) Gibraltar (United Kingdom)
 */
public static final short GIBRALTAR = 80;

/**
 *(81) Glorioso Islands (France)
 */
public static final short GLORIOSOISLANDS = 81;

/**
 *(82) Greece
 */
public static final short GREECE = 82;

/**
 *(83) Greenland (Denmark)
 */
public static final short GREENLAND = 83;

/**
 *(84) Grenada
 */
public static final short GRENADA = 84;

/**
 *(85) Guadaloupe (France)
 */
public static final short GUADALOUPE = 85;

/**
 *(86) Guam (United States)
 */
public static final short GUAM = 86;

/**
 *(87) Guatemala
 */
public static final short GUATEMALA = 87;

/**
 *(88) Guernsey (United Kingdom)
 */
public static final short GUERNSEY = 88;

/**
 *(89) Guinea
 */
public static final short GUINEA = 89;

/**
 *(9) Antigua and Barbuda
 */
public static final short ANTIGUAANDBARBUDA = 9;

/**
 *(90) Guinea- Bissau
 */
public static final short GUINEABISSAU = 90;

/**
 *(91) Guyana
 */
public static final short GUYANA = 91;

/**
 *(92) Haiti
 */
public static final short HAITI = 92;

/**
 *(93) Heard Island and McDonald Islands (Australia)
 */
public static final short HEARDISLANDANDMCDONALDISLANDS = 93;

/**
 *(94) Honduras
 */
public static final short HONDURAS = 94;

/**
 *(95) Hong Kong (United Kingdom)
 */
public static final short HONGKONG = 95;

/**
 *(96) Howland Island (United States)
 */
public static final short HOWLANDISLAND = 96;

/**
 *(97) Hungary
 */
public static final short HUNGARY = 97;

/**
 *(98) Iceland
 */
public static final short ICELAND = 98;

/**
 *(99) India
 */
public static final short INDIA = 99;

/**
 * Returns a string containing the enumeration name which corresponds to an enumeration value,
 * as in <b><code>CountryField.toString (0)</code></b> returns the string "<b><code>OTHER</code></b>"
 */

public static String toString(int idNumber) 
{
	switch (idNumber) {
		case 0: return "Other"; 
		case 1: return "Afghanistan"; 
		case 10: return "Argentina"; 
		case 100: return "Indonesia"; 
		case 101: return "Iran"; 
		case 102: return "Iraq"; 
		case 104: return "Ireland"; 
		case 105: return "Israel"; 
		case 106: return "Italy"; 
		case 107: return "Cote D'Ivoire (aka Ivory Coast)"; 
		case 108: return "Jamaica"; 
		case 109: return "Jan Mayen (Norway)"; 
		case 11: return "Aruba"; 
		case 110: return "Japan"; 
		case 111: return "Jarvis Island (United States)"; 
		case 112: return "Jersey (United Kingdom)"; 
		case 113: return "Johnston Atoll (United States)"; 
		case 114: return "Jordan"; 
		case 115: return "Juan  de Nova Island"; 
		case 116: return "Kenya"; 
		case 117: return "Kingman Reef  (United States)"; 
		case 118: return "Kiribati"; 
		case 119: return "Korea, Democratic People's Republic of (North)"; 
		case 12: return "Ashmore and Cartier Islands (Australia)"; 
		case 120: return "Korea, Republic of (South)"; 
		case 121: return "Kuwait"; 
		case 122: return "Laos"; 
		case 123: return "Lebanon"; 
		case 124: return "Lesotho"; 
		case 125: return "Liberia"; 
		case 126: return "Libya"; 
		case 127: return "Liechtenstein"; 
		case 128: return "Luxembourg"; 
		case 129: return "Madagascar"; 
		case 13: return "Australia"; 
		case 130: return "Macau (Portugal)"; 
		case 131: return "Malawi"; 
		case 132: return "Malaysia"; 
		case 133: return "Maldives"; 
		case 134: return "Mali"; 
		case 135: return "Malta"; 
		case 136: return "Man, Isle of (United Kingdom)"; 
		case 137: return "Marshall Islands"; 
		case 138: return "Martinique  (France)"; 
		case 139: return "Mauritania"; 
		case 14: return "Austria"; 
		case 140: return "Mauritius"; 
		case 141: return "Mayotte (France)"; 
		case 142: return "Mexico"; 
		case 143: return "Micronesia, Federative States of"; 
		case 144: return "Monaco"; 
		case 145: return "Mongolia"; 
		case 146: return "Montserrat (United Kingdom)"; 
		case 147: return "Morocco"; 
		case 148: return "Mozambique"; 
		case 149: return "Namibia (South West Africa)"; 
		case 15: return "Bahamas"; 
		case 150: return "Nauru"; 
		case 151: return "Navassa Island (United States)"; 
		case 152: return "Nepal"; 
		case 153: return "Netherlands"; 
		case 154: return "Netherlands Antilles (Curacao, Bonaire, Saba, Sint Maarten Sint Eustatius)"; 
		case 155: return "New Caledonia (France)"; 
		case 156: return "New Zealand"; 
		case 157: return "Nicaragua"; 
		case 158: return "Niger"; 
		case 159: return "Nigeria"; 
		case 16: return "Bahrain"; 
		case 160: return "Niue (New Zealand)"; 
		case 161: return "Norfolk Island (Australia)"; 
		case 162: return "Northern Mariana Islands (United States)"; 
		case 163: return "Norway"; 
		case 164: return "Oman"; 
		case 165: return "Pakistan"; 
		case 166: return "Palmyra Atoll (United States)"; 
		case 168: return "Panama"; 
		case 169: return "Papua New Guinea"; 
		case 17: return "Baker Island (United States)"; 
		case 170: return "Paracel Islands (International - Occupied by China, also claimed by Taiwan and Vietnam)"; 
		case 171: return "Paraguay"; 
		case 172: return "Peru"; 
		case 173: return "Philippines"; 
		case 174: return "Pitcairn Islands (United Kingdom)"; 
		case 175: return "Poland"; 
		case 176: return "Portugal"; 
		case 177: return "Puerto Rico (United States)"; 
		case 178: return "Qatar"; 
		case 179: return "Reunion (France)"; 
		case 18: return "Bangladesh"; 
		case 180: return "Romania"; 
		case 181: return "Rwanda"; 
		case 182: return "St Kitts and Nevis"; 
		case 183: return "St Helena (United Kingdom)"; 
		case 184: return "St Lucia"; 
		case 185: return "St Pierre and Miquelon (France)"; 
		case 186: return "St Vincent and the Grenadines"; 
		case 187: return "San Marino"; 
		case 188: return "Sao Tome and Principe"; 
		case 189: return "Saudi Arabia"; 
		case 19: return "Barbados"; 
		case 190: return "Senegal"; 
		case 191: return "Seychelles"; 
		case 192: return "Sierra Leone"; 
		case 193: return "Singapore"; 
		case 194: return "Solomon Islands"; 
		case 195: return "Somalia"; 
		case 196: return "South Georgia and the South Sandwich Islands(United Kingdom)"; 
		case 197: return "South Africa"; 
		case 198: return "Spain"; 
		case 199: return "Spratly Islands (International - parts occupied and claimed by China,Malaysia, Philippines, Taiwan, Vietnam)"; 
		case 2: return "Albania"; 
		case 20: return "Bassas  da India (France)"; 
		case 200: return "Sri Lanka"; 
		case 201: return "Sudan"; 
		case 202: return "Suriname"; 
		case 203: return "Svalbard (Norway)"; 
		case 204: return "Swaziland"; 
		case 205: return "Sweden"; 
		case 206: return "Switzerland"; 
		case 207: return "Syria"; 
		case 208: return "Taiwan"; 
		case 209: return "Tanzania"; 
		case 21: return "Belgium"; 
		case 210: return "Thailand"; 
		case 211: return "Togo"; 
		case 212: return "Tokelau (New Zealand)"; 
		case 213: return "Tonga"; 
		case 214: return "Trinidad and Tobago"; 
		case 215: return "Tromelin Island (France)"; 
		case 216: return "Pacific Islands, Trust Territory of the (Palau)"; 
		case 217: return "Tunisia"; 
		case 218: return "Turkey"; 
		case 219: return "Turks and Caicos Islands (United Kingdom)"; 
		case 22: return "Belize"; 
		case 220: return "Tuvalu"; 
		case 221: return "Uganda"; 
		case 222: return "Commonwealth of Independent States"; 
		case 223: return "United Arab Emirates"; 
		case 224: return "United Kingdom"; 
		case 225: return "United States"; 
		case 226: return "Uruguay"; 
		case 227: return "Vanuatu"; 
		case 228: return "Vatican City (Holy See)"; 
		case 229: return "Venezuela"; 
		case 23: return "Benin (aka Dahomey)"; 
		case 230: return "Vietnam"; 
		case 231: return "Virgin Islands (United States)"; 
		case 232: return "Wake Island (United States)"; 
		case 233: return "Wallis and Futuna (France)"; 
		case 234: return "Western Sahara"; 
		case 235: return "West Bank (Israel)"; 
		case 236: return "Western Samoa"; 
		case 237: return "Yemen"; 
		case 24: return "Bermuda (United Kingdom)"; 
		case 241: return "Zaire"; 
		case 242: return "Zambia"; 
		case 243: return "Zimbabwe"; 
		case 244: return "Armenia"; 
		case 245: return "Azerbaijan"; 
		case 246: return "Belarus"; 
		case 247: return "Bosnia and Hercegovina"; 
		case 248: return "Clipperton Island (France)"; 
		case 249: return "Croatia"; 
		case 25: return "Bhutan"; 
		case 250: return "Estonia"; 
		case 251: return "Georgia"; 
		case 252: return "Kazakhstan"; 
		case 253: return "Kyrgyzstan"; 
		case 254: return "Latvia"; 
		case 255: return "Lithuania"; 
		case 256: return "Macedonia"; 
		case 257: return "Midway Islands (United States)"; 
		case 258: return "Moldova"; 
		case 259: return "Montenegro"; 
		case 26: return "Bolivia"; 
		case 260: return "Russia"; 
		case 261: return "Serbia and Montenegro (Montenegro to separate)"; 
		case 262: return "Slovenia"; 
		case 263: return "Tajikistan"; 
		case 264: return "Turkmenistan"; 
		case 265: return "Ukraine"; 
		case 266: return "Uzbekistan"; 
		case 27: return "Botswana"; 
		case 28: return "Bouvet Island (Norway)"; 
		case 29: return "Brazil"; 
		case 3: return "Algeria"; 
		case 30: return "British Indian Ocean Territory (United Kingdom)"; 
		case 31: return "British Virgin Islands (United Kingdom)"; 
		case 32: return "Brunei"; 
		case 33: return "Bulgaria"; 
		case 34: return "Burkina  (aka Burkina Faso or Upper Volta"; 
		case 35: return "Burma (Myanmar)"; 
		case 36: return "Burundi"; 
		case 37: return "Cambodia (aka Kampuchea)"; 
		case 38: return "Cameroon"; 
		case 39: return "Canada"; 
		case 4: return "American Samoa (United States)"; 
		case 40: return "Cape Verde, Republic of"; 
		case 41: return "Cayman Islands (United Kingdom)"; 
		case 42: return "Central African Republic"; 
		case 43: return "Chad"; 
		case 44: return "Chile"; 
		case 45: return "China, People's Republic of"; 
		case 46: return "Christmas Island (Australia)"; 
		case 47: return "Cocos (Keeling) Islands (Australia)"; 
		case 48: return "Colombia"; 
		case 49: return "Comoros"; 
		case 5: return "Andorra"; 
		case 50: return "Congo, Republic of"; 
		case 51: return "Cook Islands (New Zealand)"; 
		case 52: return "Coral Sea Islands (Australia)"; 
		case 53: return "Costa Rica"; 
		case 54: return "Cuba"; 
		case 55: return "Cyprus"; 
		case 56: return "Czechoslovakia (separating into Czech Republic and Slovak Republic)"; 
		case 57: return "Denmark"; 
		case 58: return "Djibouti"; 
		case 59: return "Dominica"; 
		case 6: return "Angola"; 
		case 60: return "Dominican Republic"; 
		case 61: return "Ecuador"; 
		case 62: return "Egypt"; 
		case 63: return "El Salvador"; 
		case 64: return "Equatorial Guinea"; 
		case 65: return "Ethiopia"; 
		case 66: return "Europa Island (France)"; 
		case 67: return "Falkland Islands (aka Islas Malvinas) (United Kingdom)"; 
		case 68: return "Faroe Islands (Denmark)"; 
		case 69: return "Fiji"; 
		case 7: return "Anguilla"; 
		case 70: return "Finland"; 
		case 71: return "France"; 
		case 72: return "French Guiana (France)"; 
		case 73: return "French Polynesia (France)"; 
		case 74: return "French Southern and Antarctic Islands (France)"; 
		case 75: return "Gabon"; 
		case 76: return "Gambia, The"; 
		case 77: return "Gaza Strip (Israel)"; 
		case 78: return "Germany"; 
		case 79: return "Ghana"; 
		case 8: return "Antarctica (International)"; 
		case 80: return "Gibraltar (United Kingdom)"; 
		case 81: return "Glorioso Islands (France)"; 
		case 82: return "Greece"; 
		case 83: return "Greenland (Denmark)"; 
		case 84: return "Grenada"; 
		case 85: return "Guadaloupe (France)"; 
		case 86: return "Guam (United States)"; 
		case 87: return "Guatemala"; 
		case 88: return "Guernsey (United Kingdom)"; 
		case 89: return "Guinea"; 
		case 9: return "Antigua and Barbuda"; 
		case 90: return "Guinea- Bissau"; 
		case 91: return "Guyana"; 
		case 92: return "Haiti"; 
		case 93: return "Heard Island and McDonald Islands (Australia)"; 
		case 94: return "Honduras"; 
		case 95: return "Hong Kong (United Kingdom)"; 
		case 96: return "Howland Island (United States)"; 
		case 97: return "Hungary"; 
		case 98: return "Iceland"; 
		case 99: return "India"; 
		default : return "";
	}
}//end of toString
}//End of class 
