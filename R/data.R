# ---------------------------------------------------------------------------- #
#' Lunar Months (Masa)
#'
#' Lookup vector and tibble of 12 lunar months (masa) in the Vedic calendar system.
#'
#' @format
#' - `masas`: Character vector, names of Vedic lunar months.
#' - `masas_tbl`: Tibble with columns:
#'    - `masa`: Month name (character)
#'    - `masa_index`: 1-based index (integer)
#'
#' @examples
#' masas[1]             # "Chaitra"
#' dplyr::filter(masas_tbl, masa == "Ashvija")
#'
#' @export
masas <- c(
  "Chaitra", "Vaishakha", "Jyeshtha", "Ashada", "Shravana", "Bhadrapada",
  "Ashvija", "Kartika", "Margashira", "Pushya", "Maagha", "Phalguna"
)
#' @export
masas_tbl <- tibble::tibble(
  masa = masas,
  masa_index = seq_along(masas)
)

# ---------------------------------------------------------------------------- #
#' Lunar Days (Tithi)
#'
#' Lookup vector and tibble of 30 lunar days (tithi) in the Vedic calendar system.
#'
#' @format
#' - `tithis`: Character vector, names of lunar days.
#' - `tithis_tbl`: Tibble with columns:
#'    - `tithi`: Name (character)
#'    - `tithi_index`: 1-based index (integer)
#'
#' @examples
#' tithis[15]                  # "Poornima"
#' dplyr::filter(tithis_tbl, grepl("prathama", tithi))
#'
#' @export
tithis <- c(
  "Shukla paksha prathama", "Shukla paksha dvitiya", "Shukla paksha trititya",
  "Shukla paksha chaturthi", "Shukla paksha panchami", "Shukla paksha sashti",
  "Shukla paksha saptami", "Shukla paksha ashtami", "Shukla paksha navami",
  "Shukla paksha dashmi", "Shukla paksha ekadashi", "Shukla paksha dvadashi",
  "Shukla paksha trayodashi", "Shukla paksha chaturdashi", "Poornima",
  "Krishna paksha prathama", "Krishna paksha dvitiya", "Krishna paksha trititya",
  "Krishna paksha chaturthi", "Krishna paksha panchami", "Krishna paksha sashti",
  "Krishna paksha saptami", "Krishna paksha ashtami", "Krishna paksha navami",
  "Krishna paksha dashmi", "Krishna paksha ekadashi", "Krishna paksha dvadashi",
  "Krishna paksha trayodashi", "Krishna paksha chaturdashi", "Amavasya"
)
#' @export
tithis_tbl <- tibble::tibble(
  tithi = tithis,
  tithi_index = seq_along(tithis)
)

# ---------------------------------------------------------------------------- #
#' Nakshatras
#'
#' Lookup vector and tibble of 27 nakshatras (lunar mansions) in the Vedic calendar system.
#'
#' @format
#' - `nakshatras`: Character vector, nakshatra names.
#' - `nakshatras_tbl`: Tibble with columns:
#'    - `nakshatra`: Name (character)
#'    - `nakshatra_index`: 1-based index (integer)
#'
#' @examples
#' nakshatras[10]               # "Magha"
#' dplyr::filter(nakshatras_tbl, grepl("^A", nakshatra))
#'
#' @export
nakshatras <- c(
  "Ashwini", "Bharani", "Kritika", "Rohini", "Mrigashira", "Ardra",
  "Punarvasu", "Pushya", "Ashlesha", "Magha", "Purvaphalguni", "Uttaraphalguni",
  "Hasta", "Chitra", "Swati", "Vishakha", "Anuradha", "Jyeshta", "Mula",
  "Purvashada", "Uttarashada", "Shravana", "Dhanishta", "Shatabhisha",
  "Purvabhadrapada", "Uttarabhadrapada", "Revati"
)
#' @export
nakshatras_tbl <- tibble::tibble(
  nakshatra = nakshatras,
  nakshatra_index = seq_along(nakshatras)
)

# ---------------------------------------------------------------------------- #
#' Ritus (Seasons)
#'
#' Lookup vector and tibble of 6 ritus (seasons) in the Vedic calendar system.
#'
#' @format
#' - `ritus`: Character vector, names of the seasons.
#' - `ritus_tbl`: Tibble with columns:
#'    - `ritu`: Name (character)
#'    - `ritu_index`: 1-based index (integer)
#'
#' @examples
#' ritus[2]                      # "Grishma"
#' dplyr::filter(ritus_tbl, grepl("a$", ritu))
#'
#' @export
ritus <- c("Vasanta", "Grishma", "Varsha", "Sharad", "Hemanta", "Sishira")
#' @export
ritus_tbl <- tibble::tibble(
  ritu = ritus,
  ritu_index = seq_along(ritus)
)

# ---------------------------------------------------------------------------- #
#' Samvatsaras (Years)
#'
#' Lookup vector and tibble of 60 samvatsaras (year names) in the Hindu Panchang.
#'
#' @format
#' - `samvatsars`: Character vector, samvatsara names.
#' - `samvatsars_tbl`: Tibble with columns:
#'    - `samvatsar`: Name (character)
#'    - `samvatsar_index`: 1-based index (integer)
#'
#' @examples
#' samvatsars[1]                  # "Prabhava"
#' dplyr::filter(samvatsars_tbl, grepl("krit", samvatsar, ignore.case = TRUE))
#'
#' @export
samvatsars <- c(
  "Prabhava", "Vibhava", "Sukla", "Pramoda", "Prajapati", "Angirasa",
  "Srimukha", "Bhava", "Yuva", "Dhatri", "Ishvara", "Bahudhanya", "Pramadhi",
  "Vikrama", "Vrushapraja", "Citrabhanu", "Subhanu", "Tarana", "Parthiva",
  "Vyaya", "Sarvajit", "Sarvadharin", "Virodhin", "Vikriti", "Khara",
  "Nandana", "Vijaya", "Jaya", "Manmatha", "Durmukhi", "Hevilambi",
  "Vilambi", "Vikari", "Sharvari", "Plava", "Shubhakrit", "Shobhakrit",
  "Krodhi", "Vishvavasu", "Parabhava", "Plavanga", "Kilaka", "Saumya",
  "Sadharana", "Virodhakruta", "Paridhavi", "Pramadi", "Ananda",
  "Rakshasa", "Nala", "Pingala", "Kalayukta", "Siddharti", "Raudra",
  "Durmati", "Dundubhi", "Rudhirodhgari", "Raktakshi", "Krodhana", "Akshaya"
)
#' @export
samvatsars_tbl <- tibble::tibble(
  samvatsar = samvatsars,
  samvatsar_index = seq_along(samvatsars)
)

# ---------------------------------------------------------------------------- #
#' Yogas
#'
#' Lookup vector and tibble of 27 yogas (astrological combinations) in the Vedic system.
#'
#' @format
#' - `yogas`: Character vector, yoga names.
#' - `yogas_tbl`: Tibble with columns:
#'    - `yoga`: Name (character)
#'    - `yoga_index`: 1-based index (integer)
#'
#' @examples
#' yogas[1]                       # "Vishkhamba"
#' dplyr::filter(yogas_tbl, grepl("ma", yoga))
#'
#' @export
yogas <- c(
  "Vishkhamba", "Preeti", "Ayushmaan", "Saubhaagya", "Sobhana", "Atiganda",
  "Sukarman", "Dhriti", "Shoola", "Ganda", "Vriddhi", "Dhruva", "Vyaaghaata",
  "Harshana", "Vajra", "Siddhi", "Vyatipaata", "Variyan", "Parigha", "Shiva",
  "Siddha", "Saadhya", "Subha", "Sukla", "Brahma", "Indra", "Vaidhriti"
)
#' @export
yogas_tbl <- tibble::tibble(
  yoga = yogas,
  yoga_index = seq_along(yogas)
)

# ---------------------------------------------------------------------------- #
#' Karanas
#'
#' Lookup vector and tibble of 60 karanas (half lunar days) in the Vedic system.
#'
#' @format
#' - `karanas`: Character vector, karana names (length 60; repeats expected).
#' - `karanas_tbl`: Tibble with columns:
#'    - `karana`: Name (character)
#'    - `karana_index`: 1-based index (integer)
#'
#' @examples
#' karanas[1:8]
#' dplyr::count(karanas_tbl, karana)
#'
#' @export
karanas <- c(
  "Kimstughna", "Bava", "Baalava", "Kaulava", "Taitila", "Garaja", "Vanija", "Visti",
  "Bava", "Baalava", "Kaulava", "Taitila", "Garaja", "Vanija", "Visti",
  "Bava", "Baalava", "Kaulava", "Taitila", "Garaja", "Vanija", "Visti",
  "Bava", "Baalava", "Kaulava", "Taitila", "Garaja", "Vanija", "Visti",
  "Bava", "Baalava", "Kaulava", "Taitila", "Garaja", "Vanija", "Visti",
  "Bava", "Baalava", "Kaulava", "Taitila", "Garaja", "Vanija", "Visti",
  "Bava", "Baalava", "Kaulava", "Taitila", "Garaja", "Vanija", "Visti",
  "Sakuni", "Chatushpada", "Naga"
)
#' @export
karanas_tbl <- tibble::tibble(
  karana = karanas,
  karana_index = seq_along(karanas)
)

# ---------------------------------------------------------------------------- #
#' Rashis (Zodiac Signs)
#'
#' Lookup vector and tibble of 12 rashis (zodiac signs).
#'
#' @format
#' - `rashis`: Character vector, rashi names.
#' - `rashis_tbl`: Tibble with columns:
#'    - `rashi`: Name (character)
#'    - `rashi_index`: 1-based index (integer)
#'
#' @examples
#' rashis[6]                      # "Kanya"
#' dplyr::filter(rashis_tbl, rashi %in% c("Mesha", "Meena"))
#'
#' @export
rashis <- c(
  "Mesha", "Vrushabha", "Mithuna", "Karka", "Sinha", "Kanya",
  "Tula", "Vrushchik", "Dhanu", "Makara", "Kumbha", "Meena"
)
#' @export
rashis_tbl <- tibble::tibble(
  rashi = rashis,
  rashi_index = seq_along(rashis)
)

# ---------------------------------------------------------------------------- #
#' Vaaras (Weekdays)
#'
#' Lookup vector and tibble of 7 vaaras (days of week).
#'
#' @format
#' - `vaaras`: Character vector, weekday names.
#' - `vaaras_tbl`: Tibble with columns:
#'    - `vaara`: Name (character)
#'    - `vaara_index`: 1-based index (integer)
#'
#' @examples
#' vaaras[1]                       # "Ravivar"
#' dplyr::filter(vaaras_tbl, vaara == "Guruwar")
#'
#' @export
vaaras <- c(
  "Ravivar", "Somvar", "Mangalwar", "Budhwar", "Guruwar", "Shukrawar", "Shaniwar"
)
#' @export
vaaras_tbl <- tibble::tibble(
  vaara = vaaras,
  vaara_index = seq_along(vaaras)
)

# ---------------------------------------------------------------------------- #
