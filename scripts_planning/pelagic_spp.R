# remove certain pelagic species due to
# 1 - very sparse distribution map
# and/or
# 2 - they will not benefit from spatial protection apart from at aggregation spots
# these were checked using fishbase as well as geremy's database
pelagic_species = c(
  #SHARKS
  # the two other thresher shark species have insufficient data
  'ALOPIAS VULPINUS',
  # can be found inshore but model not good due to scattered/insufficient data
  "CARCHARHINUS FALCIFORMIS",
  "CETORHINUS MAXIMUS",
  "ISURUS OXYRINCHUS",
  "LAMNA NASUS",
  "PRIONACE GLAUCA",
  "PSEUDOCARCHARIAS KAMOHARAI",
  "RHINCODON TYPUS",
  #RAYS
  "PTEROPLATYTRYGON VIOLACEA"
)

problem_species = c(
  # SHARKS
  # need to use IUCN as insufficient data
  "ACROTERIOBATUS OCELLATUS",
  "CALLORHINCHUS CAPENSIS",
  "CARCHARHINUS LEUCAS",
  "CENTROPHORUS UYATO",
  "CENTROPHORUS GRANULOSUS",
  "DALATIAS LICHA",
  "DEANIA PROFUNDORUM",
  "ETMOPTERUS BIGELOWI",
  "HEPTRANCHIAS PERLO",
  "HEXANCHUS GRISEUS", 
  "HOLOHALAELURUS PUNCTATUS",
  "SPHYRNA LEWINI",
  "SPHYRNA MOKARRAN",
  "SQUALUS ACUTIPINNIS",
  "SQUATINA AFRICANA",
  "ZAMEUS SQUAMULOSUS",
  # RAYS
  # Dipturus spp have taxonomic confuion (Ebert et al.)
  "DIPTURUS DOUTREI",
  "DIPTURUS SPRINGERI"
)
