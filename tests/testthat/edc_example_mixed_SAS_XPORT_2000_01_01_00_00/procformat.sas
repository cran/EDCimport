Proc format;

value $YESNO
'YES'='YES'
'NO'='NO';

value $TIMEZON
'(GMT-12:00) International Date Line West'='(GMT-12:00) International Date Line West'
'(UTC-11:00) Coordinated Universal Time-11'='(UTC-11:00) Coordinated Universal Time-11'
'(GMT-10:00) Aleutian Islands'='(GMT-10:00) Aleutian Islands'
'(GMT-10:00) Hawaii'='(GMT-10:00) Hawaii'
'(GMT-09:00) Alaska'='(GMT-09:00) Alaska'
'(UTC-09:00) Coordinated Universal Time-09'='(UTC-09:00) Coordinated Universal Time-09'
'(GMT-09:30) Marquesas Islands'='(GMT-09:30) Marquesas Islands'
'(GMT-08:00) Pacific Time (US & Canada)'='(GMT-08:00) Pacific Time (US & Canada)'
'(GMT-08:00) Baja California'='(GMT-08:00) Baja California'
'(UTC-08:00) Coordinated Universal Time-08'='(UTC-08:00) Coordinated Universal Time-08'
'(GMT-07:00) Mountain Time (US & Canada)'='(GMT-07:00) Mountain Time (US & Canada)'
'(GMT-07:00) Chihuahua, La Paz, Mazatlan'='(GMT-07:00) Chihuahua, La Paz, Mazatlan'
'(GMT-07:00) Arizona'='(GMT-07:00) Arizona'
'(GMT-06:00) Saskatchewan'='(GMT-06:00) Saskatchewan'
'(GMT-06:00) Central America'='(GMT-06:00) Central America'
'(GMT-06:00) Central Time (US & Canada)'='(GMT-06:00) Central Time (US & Canada)'
'(GMT-06:00) Guadalajara, Mexico City, Monterrey'='(GMT-06:00) Guadalajara, Mexico City, Monterrey'
'(GMT-06:00) Easter Island'='(GMT-06:00) Easter Island'
'(GMT-05:00) Havana'='(GMT-05:00) Havana'
'(GMT-05:00) Eastern Time (US & Canada)'='(GMT-05:00) Eastern Time (US & Canada)'
'(GMT-05:00) Chetumal'='(GMT-05:00) Chetumal'
'(GMT-05:00) Haiti'='(GMT-05:00) Haiti'
'(GMT-05:00) Bogota, Lima, Quito, Rio Branco'='(GMT-05:00) Bogota, Lima, Quito, Rio Branco'
'(GMT-05:00) Turks and Caicos'='(GMT-05:00) Turks and Caicos'
'(GMT-05:00) Indiana (East)'='(GMT-05:00) Indiana (East)'
'(GMT-04:00) Atlantic Time (Canada)'='(GMT-04:00) Atlantic Time (Canada)'
'(GMT-04:00) Cuiaba'='(GMT-04:00) Cuiaba'
'(GMT-04:00) Santiago'='(GMT-04:00) Santiago'
'(GMT-04:00) Asuncion'='(GMT-04:00) Asuncion'
'(GMT-04:00) Georgetown, La Paz, Manaus, San Juan'='(GMT-04:00) Georgetown, La Paz, Manaus, San Juan'
'(GMT-04:00) Caracas'='(GMT-04:00) Caracas'
'(GMT-03:00) City of Buenos Aires'='(GMT-03:00) City of Buenos Aires'
'(GMT-03:00) Salvador'='(GMT-03:00) Salvador'
'(GMT-03:00) Brasilia'='(GMT-03:00) Brasilia'
'(GMT-03:00) Greenland'='(GMT-03:00) Greenland'
'(GMT-03:00) Punta Arenas'='(GMT-03:00) Punta Arenas'
'(GMT-03:00) Montevideo'='(GMT-03:00) Montevideo'
'(GMT-03:00) Cayenne, Fortaleza'='(GMT-03:00) Cayenne, Fortaleza'
'(GMT-03:00) Saint Pierre and Miquelon'='(GMT-03:00) Saint Pierre and Miquelon'
'(GMT-03:00) Araguaina'='(GMT-03:00) Araguaina'
'(GMT-03:30) Newfoundland'='(GMT-03:30) Newfoundland'
'(GMT-02:00) Mid-Atlantic - Old'='(GMT-02:00) Mid-Atlantic - Old'
'(UTC-02:00) Coordinated Universal Time-02'='(UTC-02:00) Coordinated Universal Time-02'
'(GMT-01:00) Azores'='(GMT-01:00) Azores'
'(GMT-01:00) Cabo Verde Is.'='(GMT-01:00) Cabo Verde Is.'
'(UTC) Coordinated Universal Time'='(UTC) Coordinated Universal Time'
'(GMT+00:00) Dublin, Edinburgh, Lisbon, London'='(GMT+00:00) Dublin, Edinburgh, Lisbon, London'
'(GMT+00:00) Monrovia, Reykjavik'='(GMT+00:00) Monrovia, Reykjavik'
'(GMT+01:00) Casablanca'='(GMT+01:00) Casablanca'
'(GMT+01:00) Belgrade, Bratislava, Budapest, Ljubljana, Prague'='(GMT+01:00) Belgrade, Bratislava, Budapest, Ljubljana, Prague'
'(GMT+01:00) Sarajevo, Skopje, Warsaw, Zagreb'='(GMT+01:00) Sarajevo, Skopje, Warsaw, Zagreb'
'(GMT+01:00) Brussels, Copenhagen, Madrid, Paris'='(GMT+01:00) Brussels, Copenhagen, Madrid, Paris'
'(GMT+01:00) West Central Africa'='(GMT+01:00) West Central Africa'
'(GMT+01:00) Amsterdam, Berlin, Bern, Rome, Stockholm, Vienna'='(GMT+01:00) Amsterdam, Berlin, Bern, Rome, Stockholm, Vienna'
'(GMT+02:00) Chisinau'='(GMT+02:00) Chisinau'
'(GMT+02:00) Cairo'='(GMT+02:00) Cairo'
'(GMT+02:00) Helsinki, Kyiv, Riga, Sofia, Tallinn, Vilnius'='(GMT+02:00) Helsinki, Kyiv, Riga, Sofia, Tallinn, Vilnius'
'(GMT+02:00) Athens, Bucharest'='(GMT+02:00) Athens, Bucharest'
'(GMT+02:00) Jerusalem'='(GMT+02:00) Jerusalem'
'(GMT+02:00) Amman'='(GMT+02:00) Amman'
'(GMT+02:00) Tripoli'='(GMT+02:00) Tripoli'
'(GMT+02:00) Beirut'='(GMT+02:00) Beirut'
'(GMT+02:00) Windhoek'='(GMT+01:00) Windhoek'
'(GMT+02:00) Kaliningrad'='(GMT+02:00) Kaliningrad'
'(GMT+02:00) Harare, Pretoria'='(GMT+02:00) Harare, Pretoria'
'(GMT+02:00) Khartoum'='(GMT+02:00) Khartoum'
'(GMT+02:00) Damascus'='(GMT+02:00) Damascus'
'(GMT+02:00) Gaza, Hebron'='(GMT+02:00) Gaza, Hebron'
'(GMT+03:00) Kuwait, Riyadh'='(GMT+03:00) Kuwait, Riyadh'
'(GMT+03:00) Baghdad'='(GMT+03:00) Baghdad'
'(GMT+03:00) Minsk'='(GMT+03:00) Minsk'
'(GMT+03:00) Nairobi'='(GMT+03:00) Nairobi'
'(GMT+03:00) Moscow, St. Petersburg'='(GMT+03:00) Moscow, St. Petersburg'
'(GMT+03:00) Istanbul'='(GMT+03:00) Istanbul'
'(GMT+03:30) Tehran'='(GMT+03:30) Tehran'
'(GMT+04:00) Abu Dhabi, Muscat'='(GMT+04:00) Abu Dhabi, Muscat'
'(GMT+04:00) Astrakhan, Ulyanovsk'='(GMT+04:00) Astrakhan, Ulyanovsk'
'(GMT+04:00) Baku'='(GMT+04:00) Baku'
'(GMT+04:00) Yerevan'='(GMT+04:00) Yerevan'
'(GMT+04:00) Tbilisi'='(GMT+04:00) Tbilisi'
'(GMT+04:00) Port Louis'='(GMT+04:00) Port Louis'
'(GMT+04:00) Izhevsk, Samara'='(GMT+04:00) Izhevsk, Samara'
'(GMT+04:00) Saratov'='(GMT+04:00) Saratov'
'(GMT+04:30) Kabul'='(GMT+04:30) Kabul'
'(GMT+05:00) Islamabad, Karachi'='(GMT+05:00) Islamabad, Karachi'
'(GMT+05:00) Ekaterinburg'='(GMT+05:00) Ekaterinburg'
'(GMT+05:00) Ashgabat, Tashkent'='(GMT+05:00) Ashgabat, Tashkent'
'(GMT+05:30) Chennai, Kolkata, Mumbai, New Delhi'='(GMT+05:30) Chennai, Kolkata, Mumbai, New Delhi'
'(GMT+05:30) Sri Jayawardenepura'='(GMT+05:30) Sri Jayawardenepura'
'(GMT+05:45) Kathmandu'='(GMT+05:45) Kathmandu'
'(GMT+06:00) Dhaka'='(GMT+06:00) Dhaka'
'(GMT+06:00) Astana'='(GMT+06:00) Astana'
'(GMT+06:00) Omsk'='(GMT+06:00) Omsk'
'(GMT+06:30) Yangon (Rangoon)'='(GMT+06:30) Yangon (Rangoon)'
'(GMT+07:00) Barnaul, Gorno-Altaysk'='(GMT+07:00) Barnaul, Gorno-Altaysk'
'(GMT+07:00) Novosibirsk'='(GMT+07:00) Novosibirsk'
'(GMT+07:00) Krasnoyarsk'='(GMT+07:00) Krasnoyarsk'
'(GMT+07:00) Bangkok, Hanoi, Jakarta'='(GMT+07:00) Bangkok, Hanoi, Jakarta'
'(GMT+07:00) Tomsk'='(GMT+07:00) Tomsk'
'(GMT+07:00) Hovd'='(GMT+07:00) Hovd'
'(GMT+08:00) Beijing, Chongqing, Hong Kong, Urumqi'='(GMT+08:00) Beijing, Chongqing, Hong Kong, Urumqi'
'(GMT+08:00) Kuala Lumpur, Singapore'='(GMT+08:00) Kuala Lumpur, Singapore'
'(GMT+08:00) Irkutsk'='(GMT+08:00) Irkutsk'
'(GMT+08:00) Taipei'='(GMT+08:00) Taipei'
'(GMT+08:00) Ulaanbaatar'='(GMT+08:00) Ulaanbaatar'
'(GMT+08:00) Perth'='(GMT+08:00) Perth'
'(GMT+09:00) Pyongyang'='(GMT+09:00) Pyongyang'
'(GMT+08:45) Eucla'='(GMT+08:45) Eucla'
'(GMT+09:00) Seoul'='(GMT+09:00) Seoul'
'(GMT+09:00) Yakutsk'='(GMT+09:00) Yakutsk'
'(GMT+09:00) Osaka, Sapporo, Tokyo'='(GMT+09:00) Osaka, Sapporo, Tokyo'
'(GMT+09:00) Chita'='(GMT+09:00) Chita'
'(GMT+09:30) Darwin'='(GMT+09:30) Darwin'
'(GMT+09:30) Adelaide'='(GMT+09:30) Adelaide'
'(GMT+10:00) Canberra, Melbourne, Sydney'='(GMT+10:00) Canberra, Melbourne, Sydney'
'(GMT+10:00) Brisbane'='(GMT+10:00) Brisbane'
'(GMT+10:00) Vladivostok'='(GMT+10:00) Vladivostok'
'(GMT+10:00) Hobart'='(GMT+10:00) Hobart'
'(GMT+10:00) Guam, Port Moresby'='(GMT+10:00) Guam, Port Moresby'
'(GMT+10:30) Lord Howe Island'='(GMT+10:30) Lord Howe Island'
'(GMT+11:00) Bougainville Island'='(GMT+11:00) Bougainville Island'
'(GMT+11:00) Solomon Is., New Caledonia'='(GMT+11:00) Solomon Is., New Caledonia'
'(GMT+11:00) Magadan'='(GMT+11:00) Magadan'
'(GMT+11:00) Norfolk Island'='(GMT+11:00) Norfolk Island'
'(GMT+11:00) Chokurdakh'='(GMT+11:00) Chokurdakh'
'(GMT+11:00) Sakhalin'='(GMT+11:00) Sakhalin'
'(GMT+12:00) Fiji'='(GMT+12:00) Fiji'
'(GMT+12:00) Petropavlovsk-Kamchatsky - Old'='(GMT+12:00) Petropavlovsk-Kamchatsky - Old'
'(GMT+12:00) Auckland, Wellington'='(GMT+12:00) Auckland, Wellington'
'(GMT+12:00) Anadyr, Petropavlovsk-Kamchatsky'='(GMT+12:00) Anadyr, Petropavlovsk-Kamchatsky'
'(UTC+12:00) Coordinated Universal Time+12'='(UTC+12:00) Coordinated Universal Time+12'
'(GMT+12:45) Chatham Islands'='(GMT+12:45) Chatham Islands'
'(GMT+13:00) Samoa'='(GMT+13:00) Samoa'
'(GMT+13:00) Nuku alofa'='(GMT+13:00) Nuku alofa'
'(UTC+13:00) Coordinated Universal Time+13'='(UTC+13:00) Coordinated Universal Time+13'
'(GMT+14:00) Kiritimati Island'='(GMT+14:00) Kiritimati Island'
'(GMT+00:00) Sao Tome'='(GMT+00:00) Sao Tome'
'(GMT+04:00) Volgograd'='(GMT+04:00) Volgograd'
'(GMT+05:00) Qyzylorda'='(GMT+05:00) Qyzylorda'
'(GMT-07:00) Chihuahua, La Paz, Mazatlan - Old'='(GMT-07:00) Chihuahua, La Paz, Mazatlan - Old'
'(GMT-06:00) Guadalajara, Mexico City, Monterrey - Old'='(GMT-06:00) Guadalajara, Mexico City, Monterrey - Old';

value $COUNTRY
'UNITED STATES'='UNITED STATES'
'ALBANIA'='ALBANIA'
'ALGERIA'='ALGERIA'
'AMERICAN SAMOA'='AMERICAN SAMOA'
'ANDORRA'='ANDORRA'
'ANGOLA'='ANGOLA'
'ANGUILLA'='ANGUILLA'
'ANTARCTICA'='ANTARCTICA'
'ANTIGUA AND BARBUDA'='ANTIGUA AND BARBUDA'
'ARGENTINA'='ARGENTINA'
'ARMENIA'='ARMENIA'
'ARUBA'='ARUBA'
'AUSTRALIA'='AUSTRALIA'
'AUSTRIA'='AUSTRIA'
'BAHAMAS'='BAHAMAS'
'BAHRAIN'='BAHRAIN'
'BANGLADESH'='BANGLADESH'
'BARBADOS'='BARBADOS'
'BELARUS'='BELARUS'
'BELGIUM'='BELGIUM'
'BELIZE'='BELIZE'
'BERMUDA'='BERMUDA'
'BOLIVIA, PLURINATIONAL STATE OF'='BOLIVIA, PLURINATIONAL STATE OF'
'BOSNIA AND HERZEGOVINA'='BOSNIA AND HERZEGOVINA'
'BOTSWANA'='BOTSWANA'
'BRAZIL'='BRAZIL'
'BRITISH INDIAN OCEAN TERRITORY'='BRITISH INDIAN OCEAN TERRITORY'
'BRUNEI DARUSSALAM'='BRUNEI DARUSSALAM'
'BULGARIA'='BULGARIA'
'BURUNDI'='BURUNDI'
'CAMBODIA'='CAMBODIA'
'CAMEROON'='CAMEROON'
'CANADA'='CANADA'
'CAYMAN ISLANDS'='CAYMAN ISLANDS'
'CENTRAL AFRICAN REPUBLIC'='CENTRAL AFRICAN REPUBLIC'
'CHILE'='CHILE'
'CHINA'='CHINA'
'COLOMBIA'='COLOMBIA'
'CONGO'='CONGO'
'COSTA RICA'='COSTA RICA'
'COTE DIVOIRE'='COTE DIVOIRE'
'CROATIA'='CROATIA'
'CYPRUS'='CYPRUS'
'CZECH REPUBLIC'='CZECH REPUBLIC'
'DENMARK'='DENMARK'
'DOMINICA'='DOMINICA'
'DOMINICAN REPUBLIC'='DOMINICAN REPUBLIC'
'ECUADOR'='ECUADOR'
'EGYPT'='EGYPT'
'EL SALVADOR'='EL SALVADOR'
'ESTONIA'='ESTONIA'
'ETHIOPIA'='ETHIOPIA'
'FIJI'='FIJI'
'FINLAND'='FINLAND'
'FRANCE'='FRANCE'
'FRENCH POLYNESIA'='FRENCH POLYNESIA'
'FRENCH SOUTHERN TERRITORIES'='FRENCH SOUTHERN TERRITORIES'
'GAMBIA'='GAMBIA'
'GEORGIA'='GEORGIA'
'GERMANY'='GERMANY'
'GHANA'='GHANA'
'GIBRALTAR'='GIBRALTAR'
'GREECE'='GREECE'
'GREENLAND'='GREENLAND'
'GRENADA'='GRENADA'
'GUADELOUPE'='GUADELOUPE'
'GUAM'='GUAM'
'GUATEMALA'='GUATEMALA'
'GUINEA'='GUINEA'
'GUYANA'='GUYANA'
'HAITI'='HAITI'
'HONDURAS'='HONDURAS'
'HONG KONG'='HONG KONG'
'HUNGARY'='HUNGARY'
'ICELAND'='ICELAND'
'INDIA'='INDIA'
'INDONESIA'='INDONESIA'
'IRAN, ISLAMIC REPUBLIC OF'='IRAN, ISLAMIC REPUBLIC OF'
'IRAQ'='IRAQ'
'IRELAND'='IRELAND'
'ISRAEL'='ISRAEL'
'ITALY'='ITALY'
'JAMAICA'='JAMAICA'
'JAPAN'='JAPAN'
'JORDAN'='JORDAN'
'KENYA'='KENYA'
'KOREA, DEMOCRATIC PEOPLES REPUBLIC OF'='KOREA, DEMOCRATIC PEOPLES REPUBLIC OF'
'KOREA, REPUBLIC OF'='KOREA, REPUBLIC OF'
'LAO PEOPLES DEMOCRATIC REPUBLIC'='LAO PEOPLES DEMOCRATIC REPUBLIC'
'LATVIA'='LATVIA'
'LEBANON'='LEBANON'
'LIBERIA'='LIBERIA'
'LITHUANIA'='LITHUANIA'
'LUXEMBOURG'='LUXEMBOURG'
'MACAO'='MACAO'
'MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF'='MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF'
'MALAYSIA'='MALAYSIA'
'MALTA'='MALTA'
'MARSHALL ISLANDS'='MARSHALL ISLANDS'
'MARTINIQUE'='MARTINIQUE'
'MEXICO'='MEXICO'
'MONACO'='MONACO'
'MONGOLIA'='MONGOLIA'
'MOROCCO'='MOROCCO'
'NAMIBIA'='NAMIBIA'
'NEPAL'='NEPAL'
'NETHERLANDS'='NETHERLANDS'
'NEW ZEALAND'='NEW ZEALAND'
'NICARAGUA'='NICARAGUA'
'NIGER'='NIGER'
'NIGERIA'='NIGERIA'
'NORFOLK ISLAND'='NORFOLK ISLAND'
'NORWAY'='NORWAY'
'PAKISTAN'='PAKISTAN'
'PANAMA'='PANAMA'
'PAPUA NEW GUINEA'='PAPUA NEW GUINEA'
'PARAGUAY'='PARAGUAY'
'PERU'='PERU'
'PHILIPPINES'='PHILIPPINES'
'POLAND'='POLAND'
'PORTUGAL'='PORTUGAL'
'PUERTO RICO'='PUERTO RICO'
'ROMANIA'='ROMANIA'
'RUSSIAN FEDERATION'='RUSSIAN FEDERATION'
'RWANDA'='RWANDA'
'SAINT LUCIA'='SAINT LUCIA'
'SAMOA'='SAMOA'
'SAUDI ARABIA'='SAUDI ARABIA'
'SENEGAL'='SENEGAL'
'SEYCHELLES'='SEYCHELLES'
'SINGAPORE'='SINGAPORE'
'SLOVAKIA'='SLOVAKIA'
'SLOVENIA'='SLOVENIA'
'SOLOMON ISLANDS'='SOLOMON ISLANDS'
'SOUTH AFRICA'='SOUTH AFRICA'
'SPAIN'='SPAIN'
'SRI LANKA'='SRI LANKA'
'SUDAN'='SUDAN'
'SWEDEN'='SWEDEN'
'SWITZERLAND'='SWITZERLAND'
'SYRIAN ARAB REPUBLIC'='SYRIAN ARAB REPUBLIC'
'TAIWAN, PROVINCE OF CHINA'='TAIWAN, PROVINCE OF CHINA'
'TANZANIA, UNITED REPUBLIC OF'='TANZANIA, UNITED REPUBLIC OF'
'THAILAND'='THAILAND'
'TOKELAU'='TOKELAU'
'TONGA'='TONGA'
'TRINIDAD AND TOBAGO'='TRINIDAD AND TOBAGO'
'TURKEY'='TURKEY'
'UGANDA'='UGANDA'
'UKRAINE'='UKRAINE'
'UNITED ARAB EMIRATES'='UNITED ARAB EMIRATES'
'UNITED KINGDOM'='UNITED KINGDOM'
'URUGUAY'='URUGUAY'
'UZBEKISTAN'='UZBEKISTAN'
'VENEZUELA, BOLIVARIAN REPUBLIC OF'='VENEZUELA, BOLIVARIAN REPUBLIC OF'
'VIET NAM'='VIET NAM'
'VIRGIN ISLANDS, BRITISH'='VIRGIN ISLANDS, BRITISH'
'VIRGIN ISLANDS, U.S.'='VIRGIN ISLANDS, U.S.'
'ZAMBIA'='ZAMBIA'
'ZIMBABWE'='ZIMBABWE'
'Other'='Other'
'AFGHANISTAN'='AFGHANISTAN'
'ALAND ISLANDS'='ALAND ISLANDS'
'AZERBAIJAN'='AZERBAIJAN'
'BENIN'='BENIN'
'BHUTAN'='BHUTAN'
'BOUVET ISLAND'='BOUVET ISLAND'
'BURKINA FASO'='BURKINA FASO'
'CAPE VERDE'='CAPE VERDE'
'CHAD'='CHAD'
'CHRISTMAS ISLAND'='CHRISTMAS ISLAND'
'COCOS (KEELING) ISLANDS'='COCOS (KEELING) ISLANDS'
'COMOROS'='COMOROS'
'CONGO, THE DEMOCRATIC REPUBLIC OF THE'='CONGO, THE DEMOCRATIC REPUBLIC OF THE'
'COOK ISLANDS'='COOK ISLANDS'
'CUBA'='CUBA'
'DJIBOUTI'='DJIBOUTI'
'EQUATORIAL GUINEA'='EQUATORIAL GUINEA'
'ERITREA'='ERITREA'
'FALKLAND ISLANDS (MALVINAS)'='FALKLAND ISLANDS (MALVINAS)'
'FAROE ISLANDS'='FAROE ISLANDS'
'FRENCH GUIANA'='FRENCH GUIANA'
'GABON'='GABON'
'GUERNSEY'='GUERNSEY'
'GUINEA-BISSAU'='GUINEA-BISSAU'
'HEARD ISLAND AND MCDONALD ISLANDS'='HEARD ISLAND AND MCDONALD ISLANDS'
'HOLY SEE (VATICAN CITY STATE)'='HOLY SEE (VATICAN CITY STATE)'
'ISLE OF MAN'='ISLE OF MAN'
'JERSEY'='JERSEY'
'KAZAKHSTAN'='KAZAKHSTAN'
'KIRIBATI'='KIRIBATI'
'KUWAIT'='KUWAIT'
'KYRGYZSTAN'='KYRGYZSTAN'
'LESOTHO'='LESOTHO'
'LIBYAN ARAB JAMAHIRIYA'='LIBYAN ARAB JAMAHIRIYA'
'LIECHTENSTEIN'='LIECHTENSTEIN'
'MADAGASCAR'='MADAGASCAR'
'MALAWI'='MALAWI'
'MALDIVES'='MALDIVES'
'MALI'='MALI'
'MAURITANIA'='MAURITANIA'
'MAURITIUS'='MAURITIUS'
'MAYOTTE'='MAYOTTE'
'MICRONESIA, FEDERATED STATES OF'='MICRONESIA, FEDERATED STATES OF'
'MOLDOVA, REPUBLIC OF'='MOLDOVA, REPUBLIC OF'
'MONTENEGRO'='MONTENEGRO'
'MONTSERRAT'='MONTSERRAT'
'MOZAMBIQUE'='MOZAMBIQUE'
'MYANMAR'='MYANMAR'
'NAURU'='NAURU'
'NETHERLANDS ANTILLES'='NETHERLANDS ANTILLES'
'NEW CALEDONIA'='NEW CALEDONIA'
'NIUE'='NIUE'
'NORTHERN MARIANA ISLANDS'='NORTHERN MARIANA ISLANDS'
'OMAN'='OMAN'
'PALAU'='PALAU'
'PALESTINIAN TERRITORY, OCCUPIED'='PALESTINIAN TERRITORY, OCCUPIED'
'PITCAIRN'='PITCAIRN'
'QATAR'='QATAR'
'REUNION'='REUNION'
'SAINT BARTHELEMY'='SAINT BARTHELEMY'
'SAINT HELENA'='SAINT HELENA'
'SAINT KITTS AND NEVIS'='SAINT KITTS AND NEVIS'
'SAINT MARTIN'='SAINT MARTIN'
'SAINT PIERRE AND MIQUELON'='SAINT PIERRE AND MIQUELON'
'SAINT VINCENT AND THE GRENADINES'='SAINT VINCENT AND THE GRENADINES'
'SAN MARINO'='SAN MARINO'
'SAO TOME AND PRINCIPE'='SAO TOME AND PRINCIPE'
'SERBIA'='SERBIA'
'SIERRA LEONE'='SIERRA LEONE'
'SOMALIA'='SOMALIA'
'SOUTH GEORGIA AND THE SOUTH SANDWICH ISLANDS'='SOUTH GEORGIA AND THE SOUTH SANDWICH ISLANDS'
'SURINAME'='SURINAME'
'SVALBARD AND JAN MAYEN'='SVALBARD AND JAN MAYEN'
'SWAZILAND'='SWAZILAND'
'TAJIKISTAN'='TAJIKISTAN'
'TIMOR-LESTE'='TIMOR-LESTE'
'TOGO'='TOGO'
'TUNISIA'='TUNISIA'
'TURKMENISTAN'='TURKMENISTAN'
'TURKS AND CAICOS ISLANDS'='TURKS AND CAICOS ISLANDS'
'TUVALU'='TUVALU'
'UNITED STATES MINOR OUTLYING ISLANDS'='UNITED STATES MINOR OUTLYING ISLANDS'
'VANUATU'='VANUATU'
'VATICAN CITY STATE'='VATICAN CITY STATE'
'WALLIS AND FUTUNA'='WALLIS AND FUTUNA'
'WESTERN SAHARA'='WESTERN SAHARA'
'YEMEN'='YEMEN'
'VIRGIN ISLANDS (U.S.)'='VIRGIN ISLANDS (U_S_)'
'MACEDONIA, THE FORMER YUGOSLAV REPUBLIC'='MACEDONIA, THE FORMER YUGOSLAV'
'YUGOSLAVIA'='YUGOSLAVIA'
'ZAIRE'='ZAIRE';

value $MONITOR
'All forms'='All forms'
'None'='None';

value $YESNO_C
'Y'='Yes'
'N'='No'
'NA'='NA'
'U'='Unknown';

value $ISO___A
'AFG'='AFGHANISTAN'
'ALA'='ALAND ISLANDS'
'ALB'='ALBANIA'
'DZA'='ALGERIA'
'ASM'='AMERICAN SAMOA'
'AND'='ANDORRA'
'AGO'='ANGOLA'
'AIA'='ANGUILLA'
'ATA'='ANTARCTICA'
'ATG'='ANTIGUA AND BARBUDA'
'ARG'='ARGENTINA'
'ARM'='ARMENIA'
'ABW'='ARUBA'
'AUS'='AUSTRALIA'
'AUT'='AUSTRIA'
'AZE'='AZERBAIJAN'
'BHS'='BAHAMAS'
'BHR'='BAHRAIN'
'BGD'='BANGLADESH'
'BRB'='BARBADOS'
'BLR'='BELARUS'
'BEL'='BELGIUM'
'BLZ'='BELIZE'
'BEN'='BENIN'
'BMU'='BERMUDA'
'BTN'='BHUTAN'
'BOL'='BOLIVIA, PLURINATIONAL STATE OF'
'BES'='BONAIRE, SINT EUSTATIUS AND SABA'
'BIH'='BOSNIA AND HERZEGOVINA'
'BWA'='BOTSWANA'
'BVT'='BOUVET ISLAND'
'BRA'='BRAZIL'
'IOT'='BRITISH INDIAN OCEAN TERRITORY'
'BRN'='BRUNEI DARUSSALAM'
'BGR'='BULGARIA'
'BFA'='BURKINA FASO'
'BDI'='BURUNDI'
'CPV'='CABO VERDE'
'KHM'='CAMBODIA'
'CMR'='CAMEROON'
'CAN'='CANADA'
'CYM'='CAYMAN ISLANDS'
'CAF'='CENTRAL AFRICAN REPUBLIC'
'TCD'='CHAD'
'CHL'='CHILE'
'CHN'='CHINA'
'CXR'='CHRISTMAS ISLAND'
'CCK'='COCOS (KEELING) ISLANDS'
'COL'='COLOMBIA'
'COM'='COMOROS'
'COG'='CONGO'
'COD'='CONGO, THE DEMOCRATIC REPUBLIC OF'
'COK'='COOK ISLANDS'
'CRI'='COSTA RICA'
'CIV'='COTE D''IVOIRE'
'HRV'='CROATIA'
'CUB'='CUBA'
'CUW'='CURACAO'
'CYP'='CYPRUS'
'CZE'='CZECH REPUBLIC'
'DNK'='DENMARK'
'DJI'='DJIBOUTI'
'DMA'='DOMINICA'
'DOM'='DOMINICAN REPUBLIC'
'ECU'='ECUADOR'
'EGY'='EGYPT'
'SLV'='EL SALVADOR'
'GNQ'='EQUATORIAL GUINEA'
'ERI'='ERITREA'
'EST'='ESTONIA'
'ETH'='ETHIOPIA'
'FLK'='FALKLAND ISLANDS (MALVINAS)'
'FRO'='FAROE ISLANDS'
'FJI'='FIJI'
'FIN'='FINLAND'
'FRA'='FRANCE'
'GUF'='FRENCH GUIANA'
'PYF'='FRENCH POLYNESIA'
'ATF'='FRENCH SOUTHERN TERRITORIES'
'GAB'='GABON'
'GMB'='GAMBIA'
'GEO'='GEORGIA'
'DEU'='GERMANY'
'GHA'='GHANA'
'GIB'='GIBRALTAR'
'GRC'='GREECE'
'GRL'='GREENLAND'
'GRD'='GRENADA'
'GLP'='GUADELOUPE'
'GUM'='GUAM'
'GTM'='GUATEMALA'
'GGY'='GUERNSEY'
'GIN'='GUINEA'
'GNB'='GUINEA-BISSAU'
'GUY'='GUYANA'
'HTI'='HAITI'
'HMD'='HEARD ISLAND AND MCDONALD ISLANDS'
'HND'='HONDURAS'
'HKG'='HONG KONG'
'HUN'='HUNGARY'
'ISL'='ICELAND'
'IND'='INDIA'
'IDN'='INDONESIA'
'IRN'='IRAN (ISLAMIC REPUBLIC OF)'
'IRQ'='IRAQ'
'IRL'='IRELAND'
'IMN'='ISLE OF MAN'
'ISR'='ISRAEL'
'ITA'='ITALY'
'JAM'='JAMAICA'
'JPN'='JAPAN'
'JEY'='JERSEY'
'JOR'='JORDAN'
'KAZ'='KAZAKHSTAN'
'KEN'='KENYA'
'KIR'='KIRIBATI'
'PRK'='KOREA, DEMOCRATIC PEOPLE''S REPUBLIC OF'
'KOR'='KOREA, REPUBLIC OF'
'KWT'='KUWAIT'
'KGZ'='KYRGYZSTAN'
'LAO'='LAO PEOPLE''S DEMOCRATIC REPUBLIC'
'LVA'='LATVIA'
'LBN'='LEBANON'
'LSO'='LESOTHO'
'LBR'='LIBERIA'
'LBY'='LIBYA'
'LIE'='LIECHTENSTEIN'
'LTU'='LITHUANIA'
'LUX'='LUXEMBOURG'
'MAC'='MACAO'
'MKD'='MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF'
'MDG'='MADAGASCAR'
'MWI'='MALAWI'
'MYS'='MALAYSIA'
'MDV'='MALDIVES'
'MLI'='MALI'
'MLT'='MALTA'
'MHL'='MARSHALL ISLANDS'
'MTQ'='MARTINIQUE'
'MRT'='MAURITANIA'
'MUS'='MAURITIUS'
'MYT'='MAYOTTE'
'MEX'='MEXICO'
'FSM'='MICRONESIA, FEDERATED STATES OF'
'MDA'='MOLDOVA, REPUBLIC OF'
'MCO'='MONACO'
'MNG'='MONGOLIA'
'MNE'='MONTENEGRO'
'MSR'='MONTSERRAT'
'MAR'='MOROCCO'
'MOZ'='MOZAMBIQUE'
'MMR'='MYANMAR'
'NAM'='NAMIBIA'
'NRU'='NAURU'
'NPL'='NEPAL'
'NLD'='NETHERLANDS'
'NCL'='NEW CALEDONIA'
'NZL'='NEW ZEALAND'
'NIC'='NICARAGUA'
'NER'='NIGER'
'NGA'='NIGERIA'
'NIU'='NIUE'
'NFK'='NORFOLK ISLAND'
'MNP'='NORTHERN MARIANA ISLANDS'
'NOR'='NORWAY'
'OMN'='OMAN'
'PAK'='PAKISTAN'
'PLW'='PALAU'
'PSE'='PALESTINE, STATE OF'
'PAN'='PANAMA'
'PNG'='PAPUA NEW GUINEA'
'PRY'='PARAGUAY'
'PER'='PERU'
'PHL'='PHILIPPINES'
'PCN'='PITCAIRN'
'POL'='POLAND'
'PRT'='PORTUGAL'
'PRI'='PUERTO RICO'
'QAT'='QATAR'
'REU'='REUNION'
'ROU'='ROMANIA'
'RUS'='RUSSIAN FEDERATION'
'RWA'='RWANDA'
'BLM'='SAINT BARTHELEMY'
'SHN'='SAINT HELENA, ASCENSION AND TRISTAN DA CUNHA'
'KNA'='SAINT KITTS AND NEVIS'
'LCA'='SAINT LUCIA'
'MAF'='SAINT MARTIN (FRENCH PART)'
'SPM'='SAINT PIERRE AND MIQUELON'
'VCT'='SAINT VINCENT AND THE GRENADINES'
'WSM'='SAMOA'
'SMR'='SAN MARINO'
'STP'='SAO TOME AND PRINCIPE'
'SAU'='SAUDI ARABIA'
'SEN'='SENEGAL'
'SRB'='SERBIA'
'SYC'='SEYCHELLES'
'SLE'='SIERRA LEONE'
'SGP'='SINGAPORE'
'SXM'='SINT MAARTEN (DUTCH PART)'
'SVK'='SLOVAKIA'
'SVN'='SLOVENIA'
'SLB'='SOLOMON ISLANDS'
'SOM'='SOMALIA'
'ZAF'='SOUTH AFRICA'
'SGS'='SOUTH GEORGIA AND THE SOUTH SANDWICH ISLANDS'
'SSD'='SOUTH SUDAN'
'ESP'='SPAIN'
'LKA'='SRI LANKA'
'SDN'='SUDAN'
'SUR'='SURINAME'
'SJM'='SVALBARD AND JAN MAYEN'
'SWZ'='SWAZILAND'
'SWE'='SWEDEN'
'CHE'='SWITZERLAND'
'SYR'='SYRIAN ARAB REPUBLIC'
'TWN'='TAIWAN, PROVINCE OF CHINA'
'TJK'='TAJIKISTAN'
'TZA'='TANZANIA, UNITED REPUBLIC OF'
'THA'='THAILAND'
'TLS'='TIMOR-LESTE'
'TGO'='TOGO'
'TKL'='TOKELAU'
'TON'='TONGA'
'TTO'='TRINIDAD AND TOBAGO'
'TUN'='TUNISIA'
'TUR'='TURKEY'
'TKM'='TURKMENISTAN'
'TCA'='TURKS AND CAICOS ISLANDS'
'TUV'='TUVALU'
'UGA'='UGANDA'
'UKR'='UKRAINE'
'ARE'='UNITED ARAB EMIRATES'
'GBR'='UNITED KINGDOM'
'USA'='UNITED STATES'
'UMI'='UNITED STATES MINOR OUTLYING ISLANDS'
'URY'='URUGUAY'
'UZB'='UZBEKISTAN'
'VUT'='VANUATU'
'VAT'='VATICAN CITY STATE'
'VEN'='VENEZUELA, BOLIVARIAN REPUBLIC OF'
'VNM'='VIET NAM'
'VGB'='VIRGIN ISLANDS, BRITISH'
'VIR'='VIRGIN ISLANDS, U.S.'
'WLF'='WALLIS AND FUTUNA'
'ESH'='WESTERN SAHARA'
'YEM'='YEMEN'
'ZMB'='ZAMBIA'
'ZWE'='ZIMBABWE';

value $ISO___B
'AF'='AFGHANISTAN'
'AX'='ALAND ISLANDS'
'AL'='ALBANIA'
'DZ'='ALGERIA'
'AS'='AMERICAN SAMOA'
'AD'='ANDORRA'
'AO'='ANGOLA'
'AI'='ANGUILLA'
'AQ'='ANTARCTICA'
'AG'='ANTIGUA AND BARBUDA'
'AR'='ARGENTINA'
'AM'='ARMENIA'
'AW'='ARUBA'
'AU'='AUSTRALIA'
'AT'='AUSTRIA'
'AZ'='AZERBAIJAN'
'BS'='BAHAMAS'
'BH'='BAHRAIN'
'BD'='BANGLADESH'
'BB'='BARBADOS'
'BY'='BELARUS'
'BE'='BELGIUM'
'BZ'='BELIZE'
'BJ'='BENIN'
'BM'='BERMUDA'
'BT'='BHUTAN'
'BO'='BOLIVIA, PLURINATIONAL STATE OF'
'BQ'='BONAIRE, SINT EUSTATIUS AND SABA'
'BA'='BOSNIA AND HERZEGOVINA'
'BW'='BOTSWANA'
'BV'='BOUVET ISLAND'
'BR'='BRAZIL'
'IO'='BRITISH INDIAN OCEAN TERRITORY'
'BN'='BRUNEI DARUSSALAM'
'BG'='BULGARIA'
'BF'='BURKINA FASO'
'BI'='BURUNDI'
'CV'='CABO VERDE'
'KH'='CAMBODIA'
'CM'='CAMEROON'
'CA'='CANADA'
'KY'='CAYMAN ISLANDS'
'CF'='CENTRAL AFRICAN REPUBLIC'
'TD'='CHAD'
'CL'='CHILE'
'CN'='CHINA'
'CX'='CHRISTMAS ISLAND'
'CC'='COCOS (KEELING) ISLANDS'
'CO'='COLOMBIA'
'KM'='COMOROS'
'CG'='CONGO'
'CD'='CONGO, THE DEMOCRATIC REPUBLIC OF'
'CK'='COOK ISLANDS'
'CR'='COSTA RICA'
'CI'='COTE D''IVOIRE'
'HR'='CROATIA'
'CU'='CUBA'
'CW'='CURACAO'
'CY'='CYPRUS'
'CZ'='CZECH REPUBLIC'
'DK'='DENMARK'
'DJ'='DJIBOUTI'
'DM'='DOMINICA'
'DO'='DOMINICAN REPUBLIC'
'EC'='ECUADOR'
'EG'='EGYPT'
'SV'='EL SALVADOR'
'GQ'='EQUATORIAL GUINEA'
'ER'='ERITREA'
'EE'='ESTONIA'
'ET'='ETHIOPIA'
'FK'='FALKLAND ISLANDS (MALVINAS)'
'FO'='FAROE ISLANDS'
'FJ'='FIJI'
'FI'='FINLAND'
'FR'='FRANCE'
'GF'='FRENCH GUIANA'
'PF'='FRENCH POLYNESIA'
'TF'='FRENCH SOUTHERN TERRITORIES'
'GA'='GABON'
'GM'='GAMBIA'
'GE'='GEORGIA'
'DE'='GERMANY'
'GH'='GHANA'
'GI'='GIBRALTAR'
'GR'='GREECE'
'GL'='GREENLAND'
'GD'='GRENADA'
'GP'='GUADELOUPE'
'GU'='GUAM'
'GT'='GUATEMALA'
'GG'='GUERNSEY'
'GN'='GUINEA'
'GW'='GUINEA-BISSAU'
'GY'='GUYANA'
'HT'='HAITI'
'HM'='HEARD ISLAND AND MCDONALD ISLANDS'
'HN'='HONDURAS'
'HK'='HONG KONG'
'HU'='HUNGARY'
'IS'='ICELAND'
'IN'='INDIA'
'ID'='INDONESIA'
'IR'='IRAN (ISLAMIC REPUBLIC OF)'
'IQ'='IRAQ'
'IE'='IRELAND'
'IM'='ISLE OF MAN'
'IL'='ISRAEL'
'IT'='ITALY'
'JM'='JAMAICA'
'JP'='JAPAN'
'JE'='JERSEY'
'JO'='JORDAN'
'KZ'='KAZAKHSTAN'
'KE'='KENYA'
'KI'='KIRIBATI'
'KP'='KOREA, DEMOCRATIC PEOPLE''S REPUBLIC OF'
'KR'='KOREA, REPUBLIC OF'
'KW'='KUWAIT'
'KG'='KYRGYZSTAN'
'LA'='LAO PEOPLE''S DEMOCRATIC REPUBLIC'
'LV'='LATVIA'
'LB'='LEBANON'
'LS'='LESOTHO'
'LR'='LIBERIA'
'LY'='LIBYA'
'LI'='LIECHTENSTEIN'
'LT'='LITHUANIA'
'LU'='LUXEMBOURG'
'MO'='MACAO'
'MK'='MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF'
'MG'='MADAGASCAR'
'MW'='MALAWI'
'MY'='MALAYSIA'
'MV'='MALDIVES'
'ML'='MALI'
'MT'='MALTA'
'MH'='MARSHALL ISLANDS'
'MQ'='MARTINIQUE'
'MR'='MAURITANIA'
'MU'='MAURITIUS'
'YT'='MAYOTTE'
'MX'='MEXICO'
'FM'='MICRONESIA, FEDERATED STATES OF'
'MD'='MOLDOVA, REPUBLIC OF'
'MC'='MONACO'
'MN'='MONGOLIA'
'ME'='MONTENEGRO'
'MS'='MONTSERRAT'
'MA'='MOROCCO'
'MZ'='MOZAMBIQUE'
'MM'='MYANMAR'
'NA'='NAMIBIA'
'NR'='NAURU'
'NP'='NEPAL'
'NL'='NETHERLANDS'
'NC'='NEW CALEDONIA'
'NZ'='NEW ZEALAND'
'NI'='NICARAGUA'
'NE'='NIGER'
'NG'='NIGERIA'
'NU'='NIUE'
'NF'='NORFOLK ISLAND'
'MP'='NORTHERN MARIANA ISLANDS'
'NO'='NORWAY'
'OM'='OMAN'
'PK'='PAKISTAN'
'PW'='PALAU'
'PS'='PALESTINE, STATE OF'
'PA'='PANAMA'
'PG'='PAPUA NEW GUINEA'
'PY'='PARAGUAY'
'PE'='PERU'
'PH'='PHILIPPINES'
'PN'='PITCAIRN'
'PL'='POLAND'
'PT'='PORTUGAL'
'PR'='PUERTO RICO'
'QA'='QATAR'
'RE'='REUNION'
'RO'='ROMANIA'
'RU'='RUSSIAN FEDERATION'
'RW'='RWANDA'
'BL'='SAINT BARTHELEMY'
'SH'='SAINT HELENA, ASCENSION AND TRISTAN DA CUNHA'
'KN'='SAINT KITTS AND NEVIS'
'LC'='SAINT LUCIA'
'MF'='SAINT MARTIN (FRENCH PART)'
'PM'='SAINT PIERRE AND MIQUELON'
'VC'='SAINT VINCENT AND THE GRENADINES'
'WS'='SAMOA'
'SM'='SAN MARINO'
'ST'='SAO TOME AND PRINCIPE'
'SA'='SAUDI ARABIA'
'SN'='SENEGAL'
'RS'='SERBIA'
'SC'='SEYCHELLES'
'SL'='SIERRA LEONE'
'SG'='SINGAPORE'
'SX'='SINT MAARTEN (DUTCH PART)'
'SK'='SLOVAKIA'
'SI'='SLOVENIA'
'SB'='SOLOMON ISLANDS'
'SO'='SOMALIA'
'ZA'='SOUTH AFRICA'
'GS'='SOUTH GEORGIA AND THE SOUTH SANDWICH ISLANDS'
'SS'='SOUTH SUDAN'
'ES'='SPAIN'
'LK'='SRI LANKA'
'SD'='SUDAN'
'SR'='SURINAME'
'SJ'='SVALBARD AND JAN MAYEN'
'SZ'='SWAZILAND'
'SE'='SWEDEN'
'CH'='SWITZERLAND'
'SY'='SYRIAN ARAB REPUBLIC'
'TW'='TAIWAN, PROVINCE OF CHINA'
'TJ'='TAJIKISTAN'
'TZ'='TANZANIA, UNITED REPUBLIC OF'
'TH'='THAILAND'
'TL'='TIMOR-LESTE'
'TG'='TOGO'
'TK'='TOKELAU'
'TO'='TONGA'
'TT'='TRINIDAD AND TOBAGO'
'TN'='TUNISIA'
'TR'='TURKEY'
'TM'='TURKMENISTAN'
'TC'='TURKS AND CAICOS ISLANDS'
'TV'='TUVALU'
'UG'='UGANDA'
'UA'='UKRAINE'
'AE'='UNITED ARAB EMIRATES'
'GB'='UNITED KINGDOM'
'US'='UNITED STATES'
'UM'='UNITED STATES MINOR OUTLYING ISLANDS'
'UY'='URUGUAY'
'UZ'='UZBEKISTAN'
'VU'='VANUATU'
'VA'='VATICAN CITY STATE'
'VE'='VENEZUELA, BOLIVARIAN REPUBLIC OF'
'VN'='VIET NAM'
'VG'='VIRGIN ISLANDS, BRITISH'
'VI'='VIRGIN ISLANDS, U.S.'
'WF'='WALLIS AND FUTUNA'
'EH'='WESTERN SAHARA'
'YE'='YEMEN'
'ZM'='ZAMBIA'
'ZW'='ZIMBABWE';

value ISO____N
004='AFGHANISTAN'
248='ALAND ISLANDS'
008='ALBANIA'
012='ALGERIA'
016='AMERICAN SAMOA'
020='ANDORRA'
024='ANGOLA'
660='ANGUILLA'
010='ANTARCTICA'
028='ANTIGUA AND BARBUDA'
032='ARGENTINA'
051='ARMENIA'
533='ARUBA'
036='AUSTRALIA'
040='AUSTRIA'
031='AZERBAIJAN'
044='BAHAMAS'
048='BAHRAIN'
050='BANGLADESH'
052='BARBADOS'
112='BELARUS'
056='BELGIUM'
084='BELIZE'
204='BENIN'
060='BERMUDA'
064='BHUTAN'
068='BOLIVIA, PLURINATIONAL STATE OF'
535='BONAIRE, SINT EUSTATIUS AND SABA'
070='BOSNIA AND HERZEGOVINA'
072='BOTSWANA'
074='BOUVET ISLAND'
076='BRAZIL'
086='BRITISH INDIAN OCEAN TERRITORY'
096='BRUNEI DARUSSALAM'
100='BULGARIA'
854='BURKINA FASO'
108='BURUNDI'
132='CABO VERDE'
116='CAMBODIA'
120='CAMEROON'
124='CANADA'
136='CAYMAN ISLANDS'
140='CENTRAL AFRICAN REPUBLIC'
148='CHAD'
152='CHILE'
156='CHINA'
162='CHRISTMAS ISLAND'
166='COCOS (KEELING) ISLANDS'
170='COLOMBIA'
174='COMOROS'
178='CONGO'
180='CONGO, THE DEMOCRATIC REPUBLIC OF'
184='COOK ISLANDS'
188='COSTA RICA'
384='COTE D''IVOIRE'
191='CROATIA'
192='CUBA'
531='CURACAO'
196='CYPRUS'
203='CZECH REPUBLIC'
208='DENMARK'
262='DJIBOUTI'
212='DOMINICA'
214='DOMINICAN REPUBLIC'
218='ECUADOR'
818='EGYPT'
222='EL SALVADOR'
226='EQUATORIAL GUINEA'
232='ERITREA'
233='ESTONIA'
231='ETHIOPIA'
238='FALKLAND ISLANDS (MALVINAS)'
234='FAROE ISLANDS'
242='FIJI'
246='FINLAND'
250='FRANCE'
254='FRENCH GUIANA'
258='FRENCH POLYNESIA'
260='FRENCH SOUTHERN TERRITORIES'
266='GABON'
270='GAMBIA'
268='GEORGIA'
276='GERMANY'
288='GHANA'
292='GIBRALTAR'
300='GREECE'
304='GREENLAND'
308='GRENADA'
312='GUADELOUPE'
316='GUAM'
320='GUATEMALA'
831='GUERNSEY'
324='GUINEA'
624='GUINEA-BISSAU'
328='GUYANA'
332='HAITI'
334='HEARD ISLAND AND MCDONALD ISLANDS'
340='HONDURAS'
344='HONG KONG'
348='HUNGARY'
352='ICELAND'
356='INDIA'
360='INDONESIA'
364='IRAN (ISLAMIC REPUBLIC OF)'
368='IRAQ'
372='IRELAND'
833='ISLE OF MAN'
376='ISRAEL'
380='ITALY'
388='JAMAICA'
392='JAPAN'
832='JERSEY'
400='JORDAN'
398='KAZAKHSTAN'
404='KENYA'
296='KIRIBATI'
408='KOREA, DEMOCRATIC PEOPLE''S REPUBLIC OF'
410='KOREA, REPUBLIC OF'
414='KUWAIT'
417='KYRGYZSTAN'
418='LAO PEOPLE''S DEMOCRATIC REPUBLIC'
428='LATVIA'
422='LEBANON'
426='LESOTHO'
430='LIBERIA'
434='LIBYA'
438='LIECHTENSTEIN'
440='LITHUANIA'
442='LUXEMBOURG'
446='MACAO'
807='MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF'
450='MADAGASCAR'
454='MALAWI'
458='MALAYSIA'
462='MALDIVES'
466='MALI'
470='MALTA'
584='MARSHALL ISLANDS'
474='MARTINIQUE'
478='MAURITANIA'
480='MAURITIUS'
175='MAYOTTE'
484='MEXICO'
583='MICRONESIA, FEDERATED STATES OF'
498='MOLDOVA, REPUBLIC OF'
492='MONACO'
496='MONGOLIA'
499='MONTENEGRO'
500='MONTSERRAT'
504='MOROCCO'
508='MOZAMBIQUE'
104='MYANMAR'
516='NAMIBIA'
520='NAURU'
524='NEPAL'
528='NETHERLANDS'
540='NEW CALEDONIA'
554='NEW ZEALAND'
558='NICARAGUA'
562='NIGER'
566='NIGERIA'
570='NIUE'
574='NORFOLK ISLAND'
580='NORTHERN MARIANA ISLANDS'
578='NORWAY'
512='OMAN'
586='PAKISTAN'
585='PALAU'
275='PALESTINE, STATE OF'
591='PANAMA'
598='PAPUA NEW GUINEA'
600='PARAGUAY'
604='PERU'
608='PHILIPPINES'
612='PITCAIRN'
616='POLAND'
620='PORTUGAL'
630='PUERTO RICO'
634='QATAR'
638='REUNION'
642='ROMANIA'
643='RUSSIAN FEDERATION'
646='RWANDA'
652='SAINT BARTHELEMY'
654='SAINT HELENA, ASCENSION AND TRISTAN DA CUNHA'
659='SAINT KITTS AND NEVIS'
662='SAINT LUCIA'
663='SAINT MARTIN (FRENCH PART)'
666='SAINT PIERRE AND MIQUELON'
670='SAINT VINCENT AND THE GRENADINES'
882='SAMOA'
674='SAN MARINO'
678='SAO TOME AND PRINCIPE'
682='SAUDI ARABIA'
686='SENEGAL'
688='SERBIA'
690='SEYCHELLES'
694='SIERRA LEONE'
702='SINGAPORE'
534='SINT MAARTEN (DUTCH PART)'
703='SLOVAKIA'
705='SLOVENIA'
090='SOLOMON ISLANDS'
706='SOMALIA'
710='SOUTH AFRICA'
239='SOUTH GEORGIA AND THE SOUTH SANDWICH ISLANDS'
728='SOUTH SUDAN'
724='SPAIN'
144='SRI LANKA'
729='SUDAN'
740='SURINAME'
744='SVALBARD AND JAN MAYEN'
748='SWAZILAND'
752='SWEDEN'
756='SWITZERLAND'
760='SYRIAN ARAB REPUBLIC'
158='TAIWAN, PROVINCE OF CHINA'
762='TAJIKISTAN'
834='TANZANIA, UNITED REPUBLIC OF'
764='THAILAND'
626='TIMOR-LESTE'
768='TOGO'
772='TOKELAU'
776='TONGA'
780='TRINIDAD AND TOBAGO'
788='TUNISIA'
792='TURKEY'
795='TURKMENISTAN'
796='TURKS AND CAICOS ISLANDS'
798='TUVALU'
800='UGANDA'
804='UKRAINE'
784='UNITED ARAB EMIRATES'
826='UNITED KINGDOM'
840='UNITED STATES'
581='UNITED STATES MINOR OUTLYING ISLANDS'
858='URUGUAY'
860='UZBEKISTAN'
548='VANUATU'
336='VATICAN CITY STATE'
862='VENEZUELA, BOLIVARIAN REPUBLIC OF'
704='VIET NAM'
092='VIRGIN ISLANDS, BRITISH'
850='VIRGIN ISLANDS, U.S.'
876='WALLIS AND FUTUNA'
732='WESTERN SAHARA'
887='YEMEN'
894='ZAMBIA'
716='ZIMBABWE';

value YN
1='Yes'
0='No';

value NOYES
1='Yes'
0='No';

value SEX
1='Male'
2='Female';

value DND
0='Not done'
1='Done';

RUN;