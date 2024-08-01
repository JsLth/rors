info_table <- function(type) {
  switch(
    type,
    steepness = tibble::tibble(
      value = -5:5,
      encoding = c(
        ">=16% decline", "10% - <16% decline", "7% - <10% decline",
        "4% - <7% decline", "1% - <4% decline", "0% - <1% incline",
        "1% - <4% incline", "4% - <7% incline", "7% - <10% incline",
        "10% - <16% incline", ">=16% incline"
      ),
    ),
    
    surface = tibble::tibble(
      value = 0:18,
      name = c(
        "Unknown", "Paved", "Unpaved", "Asphalt", "Concrete", "Cobblestone", "Metal",
        "Wood", "Compacted Gravel", "Fine Gravel", "Gravel", "Dirt", "Ground", "Ice",
        "Paving Stones", "Sand", "Woodchips", "Grass", "Grass Paver"
      ),
      tags = c(
        NA, "paved", "unpaved, woodchips, rock, rocks, stone, shells, salt",
        "asphalt, chipseal, bitmac, tarmac", "concrete, cement", NA, "metal", "wood",
        "compacted, pebblestone", NA, "gravel, fine_gravel", "dirt, earth, soil",
        "ground, mud", "ice, snow",
        "paving_stones, paved_stones, sett, cobblestone, unhewn_cobblestone, bricks, brick",
        "sand", NA, "grass", "grass_paver"
      ),
    ),
    
    waycategory = tibble::tibble(
      value = c(0L, 1L, 2L, 4L, 8L, 16L),
      name = c("No category", "Highway", "Tollways", "Steps", "Ferry", "Ford"),
      tags = c(
        NA, "highway=motorway, highway=motorway_link", "toll*=yes", "highway=steps",
        "route=shuttle_train, route=ferry", "ford=yes"
      ),
    ),
    
    waytype = tibble::tibble(
      value = 0:10,
      name = c(
        "Unknown", "State Road", "Road", "Street", "Path", "Track", "Cycleway",
        "Footway", "Steps", "Ferry", "Construction"
      ),
      tags = c(
        NA, "primary, primary_link, motorway, motorway_link, trunk, trunk_link",
        "secondary, secondary_link, tertiary, tertiary_link, road, unclassified",
        "residential, service, living_street", "path", "track", "cycleway",
        "footway, pedestrian, crossing", "steps", "route=shuttle_train, route=ferry",
        "construction"
      ),
    ),
    
    trail_difficulty = tibble::tibble(
      value = 0:7,
      foot = c(
        "no tag", "sac_scale=hiking", "sac_scale=mountain_hiking",
        "sac_scale=demanding_mountain_hiking", "sac_scale=alpine_hiking",
        "sac_scale=demanding_alpine_hiking", "sac_scale=difficult_alpine_hiking", NA
      ),
      cycling = c(
        "no tag", "mtb:scale=0", "mtb:scale=1", "mtb:scale=2", "mtb:scale=3",
        "mtb:scale=4", "mtb:scale=5", "mtb:scale=6"
      ),
    ),
    
    road_access_restrictions = tibble::tibble(
      value = c(0L, 1L, 2L, 4L, 8L, 16L, 32L),
      encoding = c(
        "None (there are no restrictions)", "No", "Customers", "Destination",
        "Delivery", "Private", "Permissive"
      ),
    ),
    
    country_list = tibble::tibble(
      country_id = 1:236,
      name = c(
        "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Anguilla",
        "Antigua and Barbuda", "Argentina", "Armenia", "Australia", "Austria",
        "Azerbaijan", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium",
        "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia", "Bosnia and Herzegovina",
        "Botswana", "Brazil", "British Indian Ocean Territory",
        "British Sovereign Base Areas", "British Virgin Islands", "Brunei",
        "Bulgaria", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Canada",
        "Cape Verde", "Cayman Islands", "Central African Republic", "Chad", "Chile",
        "China", "Colombia", "Comoros", "Congo-Brazzaville", "Congo-Kinshasa",
        "Cook Islands", "Costa Rica", "C\U{F4}te d'Ivoire", "Croatia", "Cuba",
        "Cyprus", "Czech Republic", "Denmark", "Djibouti", "Dominica",
        "Dominican Republic", "East Timor", "Ecuador", "Egypt", "El Salvador",
        "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Falkland Islands",
        "Faroe Islands", "Federated States of Micronesia", "Fiji", "Finland",
        "France", "Gabon", "Gambia", "Georgia", "Germany", "Germany - Belgium",
        "Ghana", "Gibraltar", "Greece", "Greenland", "Grenada", "Guatemala",
        "Guernsey", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras",
        "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland",
        "Isle of Man", "Israel", "Italy", "Jamaica", "Jangy-ayyl", "Japan", "Jersey",
        "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kosovo", "Kuwait", "Kyrgyzstan",
        "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein",
        "Lithuania", "Luxembourg", "Macedonia", "Madagascar", "Malawi", "Malaysia",
        "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius",
        "Mexico", "Moldova", "Monaco", "Mongolia", "Montenegro", "Montserrat",
        "Morocco", "Mozambique", "Myanmar", "name:en", "Namibia", "Nauru", "Nepal",
        "Netherlands - Belgium", "New Zealand", "Nicaragua", "Niger", "Nigeria",
        "Niue", "North Korea", "Norway", "Oman", "Pakistan", "Palau",
        "Palestinian Territories", "Panama", "Papua New Guinea", "Paraguay", "Peru",
        "Philippines", "Pitcairn Islands", "Poland", "Portugal", "Qatar", "Romania",
        "Russian Federation", "Rwanda", "Sahrawi Arab Democratic Republic",
        "Saint Helena - Ascension and Tristan da Cunha", "Saint Kitts and Nevis",
        "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "San Marino",
        "S\U{E3}o Tom\U{E9} and Pr\U{ED}ncipe", "Saudi Arabia", "Senegal", "Serbia",
        "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia",
        "Solomon Islands", "Somalia", "South Africa",
        "South Georgia and the South Sandwich Islands", "South Korea", "South Sudan",
        "Spain", "Sri Lanka", "Sudan", "Suriname", "Swaziland", "Sweden",
        "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand",
        "The Bahamas", "The Netherlands", "Togo", "Tokelau", "Tonga",
        "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan",
        "Turks and Caicos Islands", "Tuvalu", "Uganda", "Ukraine",
        "United Arab Emirates", "United Kingdom", "United States of America",
        "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City", "Venezuela", "Vietnam",
        "Yemen", "Zambia", "Zimbabwe", "Border India - Bangladesh", "\U{CE}le Verte",
        "Border Azerbaijan - Armenia (Enclave AZE)", "Freezland Rock", "Border SI-HR",
        "Willis Island", "Chong-Kara",
        "\U{395}\U{3BB}\U{3BB}\U{3AC}\U{3B4}\U{3B1} - \U{3A0}\U{3B1}\U{3B3}\U{3B3}\U{3B1}\U{3AF}\U{3BF}",
        "Bristol Island", "Dist. Judges Court", "Border Kyrgyzstan - Uzbekistan",
        "Border Malawi - Mozambique", "\U{4E2D}\U{83EF}\U{6C11}\U{570B}"
      ),
    ),
    NULL
  )
}