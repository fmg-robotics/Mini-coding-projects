# Finds the wordle word for a specific day or finds when a wordle word is used

import time
from math import floor
# list of all wordle words
wordle_words = ["found", "shall", "stove", "lowly",
                "snout", "trope", "fewer", "shawl", "natal", "comma", "foray", "scare", "stair", "black", "squad",
                "royal", "chunk", "mince", "shame", "cheek", "ample", "flair", "foyer", "cargo", "oxide", "plant",
                "olive", "inert", "askew", "heist", "shown", "zesty", "trash", "larva", "forgo", "story", "hairy",
                "train", "homer", "badge", "midst", "canny", "fetus", "butch", "farce", "slung", "tipsy", "metal",
                "yield", "delve", "being", "scour", "glass", "gamer", "scrap", "money", "hinge", "album", "vouch",
                "asset", "tiara", "crept", "bayou", "atoll", "manor", "creak", "showy", "phase", "froth", "depth",
                "gloom", "flood", "trait", "girth", "piety", "goose", "float", "donor", "atone", "primo", "apron",
                "blown", "cacao", "loser", "input", "gloat", "awful", "brink", "smite", "beady", "rusty", "retro",
                "droll", "gawky", "hutch", "pinto", "egret", "lilac", "sever", "field", "fluff", "flack", "agape",
                "voice", "stead", "stalk", "berth", "madam", "night", "bland", "liver", "wedge", "augur", "roomy",
                "wacky", "flock", "angry", "trite", "aphid", "tryst", "midge", "power", "elope", "cinch", "motto",
                "stomp", "upset", "bluff", "cramp", "quart", "coyly", "youth", "rhyme", "buggy", "alien", "smear",
                "unfit", "patty", "cling", "glean", "label", "hunky", "khaki", "poker", "gruel", "twice", "twang",
                "shrug", "treat", "waste", "merit", "woven", "needy", "clown", "widow", "irony", "ruder", "gauze",
                "chief", "onset", "prize", "fungi", "charm", "gully", "inter", "whoop", "taunt", "leery", "class",
                "theme", "lofty", "tibia", "booze", "alpha", "thyme", "doubt", "parer", "chute", "stick", "trice",
                "alike", "recap", "saint", "glory", "grate", "admit", "brisk", "soggy", "usurp", "scald", "scorn",
                "leave", "twine", "sting", "bough", "marsh", "sloth", "dandy", "vigor", "howdy", "enjoy", "valid",
                "ionic", "equal", "floor", "catch", "spade", "stein", "exist", "quirk", "denim", "grove", "spiel",
                "mummy", "fault", "foggy", "flout", "carry", "sneak", "libel", "waltz", "aptly", "piney", "inept",
                "aloud", "photo", "dream", "stale", "unite", "snarl", "baker", "there", "glyph", "pooch", "hippy",
                "spell", "folly", "louse", "gulch", "vault", "godly", "threw", "fleet", "grave", "inane", "shock",
                "crave", "spite", "valve", "skimp", "claim", "rainy", "musty", "pique", "daddy", "quasi", "arise",
                "aging", "valet", "opium", "avert", "stuck", "recut", "mulch", "genre", "plume", "rifle", "count",
                "incur", "total", "wrest", "mocha", "deter", "study", "lover", "safer", "rivet", "funny", "smoke",
                "mound", "undue", "sedan", "pagan", "swine", "guile", "gusty", "equip", "tough", "canoe", "chaos",
                "covet", "human", "udder", "lunch", "blast", "stray", "manga", "melee", "lefty", "quick", "paste",
                "given", "octet", "risen", "groan", "leaky", "grind", "carve", "loose", "sadly", "spilt", "apple",
                "slack", "honey", "final", "sheen", "eerie", "minty", "slick", "derby", "wharf", "spelt", "coach",
                "erupt", "singe", "price", "spawn", "fairy", "jiffy", "filmy", "stack", "chose", "sleep", "ardor",
                "nanny", "niece", "woozy", "handy", "grace", "ditto", "stank", "cream", "usual", "diode", "valor",
                "angle", "ninja", "muddy", "chase", "reply", "prone", "spoil", "heart", "shade", "diner", "arson",
                "onion", "sleet", "dowel", "couch", "palsy", "bowel", "smile", "evoke", "creek", "lance", "eagle",
                "idiot", "siren", "built", "embed", "award", "dross", "annul", "goody", "frown", "patio", "laden",
                "humid", "elite", "lymph", "edify", "might", "reset", "visit", "gusto", "purse", "vapor", "crock",
                "write", "sunny", "loath", "chaff", "slide", "queer", "venom", "stamp", "sorry", "still", "acorn",
                "aping", "pushy", "tamer", "hater", "mania", "awoke", "brawn", "swift", "exile", "birch", "lucky",
                "freer", "risky", "ghost", "plier", "lunar", "winch", "snare", "nurse", "house", "borax", "nicer",
                "lurch", "exalt", "about", "savvy", "toxin", "tunic", "pried", "inlay", "chump", "lanky", "cress",
                "eater", "elude", "cycle", "kitty", "boule", "moron", "tenet", "place", "lobby", "plush", "vigil",
                "index", "blink", "clung", "qualm", "croup", "clink", "juicy", "stage", "decay", "nerve", "flier",
                "shaft", "crook", "clean", "china", "ridge", "vowel", "gnome", "snuck", "icing", "spiny", "rigor",
                "snail", "flown", "rabid", "prose", "thank", "poppy", "budge", "fiber", "moldy", "dowdy", "kneel",
                "track", "caddy", "quell", "dumpy", "paler", "swore", "rebar", "scuba", "splat", "flyer", "horny",
                "mason", "doing", "ozone", "amply", "molar", "ovary", "beset", "queue", "cliff", "magic", "truce",
                "sport", "fritz", "edict", "twirl", "verse", "llama", "eaten", "range", "whisk", "hovel", "rehab",
                "macaw", "sigma", "spout", "verve", "sushi", "dying", "fetid", "brain", "buddy", "thump", "scion",
                "candy", "chord", "basin", "march", "crowd", "arbor", "gayly", "musky", "stain", "dally", "bless",
                "bravo", "stung", "title", "ruler", "kiosk", "blond", "ennui", "layer", "fluid", "tatty", "score",
                "cutie", "zebra", "barge", "matey", "bluer", "aider", "shook", "river", "privy", "betel", "frisk",
                "bongo", "begun", "azure", "weave", "genie", "sound", "glove", "braid", "scope", "wryly", "rover",
                "assay", "ocean", "bloom", "irate", "later", "woken", "silky", "wreck", "dwelt", "slate", "smack",
                "solid", "amaze", "hazel", "wrist", "jolly", "globe", "flint", "rouse", "civil", "vista", "relax",
                "cover", "alive", "beech", "jetty", "bliss", "vocal", "often", "dolly", "eight", "joker", "since",
                "event", "ensue", "shunt", "diver", "poser", "worst", "sweep", "alley", "creed", "anime", "leafy",
                "bosom", "dunce", "stare", "pudgy", "waive", "choir", "stood", "spoke", "outgo", "delay", "bilge",
                "ideal", "clasp", "seize", "hotly", "laugh", "sieve", "block", "meant", "grape", "noose", "hardy",
                "shied", "drawl", "daisy", "putty", "strut", "burnt", "tulip", "crick", "idyll", "vixen", "furor",
                "geeky", "cough", "naive", "shoal", "stork", "bathe", "aunty", "check", "prime", "brass", "outer",
                "furry", "razor", "elect", "evict", "imply", "demur", "quota", "haven", "cavil", "swear", "crump",
                "dough", "gavel", "wagon", "salon", "nudge", "harem", "pitch", "sworn", "pupil", "excel", "stony",
                "cabin", "unzip", "queen", "trout", "polyp", "earth", "storm", "until", "taper", "enter", "child",
                "adopt", "minor", "fatty", "husky", "brave", "filet", "slime", "glint", "tread", "steal", "regal",
                "guest", "every", "murky", "share", "spore", "hoist", "buxom", "inner", "otter", "dimly", "level",
                "sumac", "donut", "stilt", "arena", "sheet", "scrub", "fancy", "slimy", "pearl", "silly", "porch",
                "dingo", "sepia", "amble", "shady", "bread", "friar", "reign", "dairy", "quill", "cross", "brood",
                "tuber", "shear", "posit", "blank", "villa", "shank", "piggy", "freak", "which", "among", "fecal",
                "shell", "would", "algae", "large", "rabbi", "agony", "amuse", "bushy", "copse", "swoon", "knife",
                "pouch", "ascot", "plane", "crown", "urban", "snide", "relay", "abide", "viola", "rajah", "straw",
                "dilly", "crash", "amass", "third", "trick", "tutor", "woody", "blurb", "grief", "disco", "where",
                "sassy", "beach", "sauna", "comic", "clued", "creep", "caste", "graze", "snuff", "frock", "gonad",
                "drunk", "prong", "lurid", "steel", "halve", "buyer", "vinyl", "utile", "smell", "adage", "worry",
                "tasty", "local", "trade", "finch", "ashen", "modal", "gaunt", "clove", "enact", "adorn", "roast",
                "speck", "sheik", "missy", "grunt", "snoop", "party", "touch", "mafia", "emcee", "array", "south",
                "vapid", "jelly", "skulk", "angst", "tubal", "lower", "crest", "sweat", "cyber", "adore", "tardy",
                "swami", "notch", "groom", "roach", "hitch", "young", "align", "ready", "frond", "strap", "puree",
                "realm", "venue", "swarm", "offer", "seven", "dryer", "diary", "dryly", "drank", "acrid", "heady",
                "theta", "junto", "pixie", "quoth", "bonus", "shalt", "penne", "amend", "datum", "build", "piano",
                "shelf", "lodge", "suing", "rearm", "coral", "ramen", "worth", "psalm", "infer", "overt", "mayor",
                "ovoid", "glide", "usage", "poise", "randy", "chuck", "prank", "fishy", "tooth", "ether", "drove",
                "idler", "swath", "stint", "while", "begat", "apply", "slang", "tarot", "radar", "credo", "aware",
                "canon", "shift", "timer", "bylaw", "serum", "three", "steak", "iliac", "shirk", "blunt", "puppy",
                "penal", "joist", "bunny", "shape", "beget", "wheel", "adept", "stunt", "stole", "topaz", "chore",
                "fluke", "afoot", "bloat", "bully", "dense", "caper", "sneer", "boxer", "jumbo", "lunge", "space",
                "avail", "short", "slurp", "loyal", "flirt", "pizza", "conch", "tempo", "droop", "plate", "bible",
                "plunk", "afoul", "savoy", "steep", "agile", "stake", "dwell", "knave", "beard", "arose", "motif",
                "smash", "broil", "glare", "shove", "baggy", "mammy", "swamp", "along", "rugby", "wager", "quack",
                "squat", "snaky", "debit", "mange", "skate", "ninth", "joust", "tramp", "spurn", "medal", "micro",
                "rebel", "flank", "learn", "nadir", "maple", "comfy", "remit", "gruff", "ester", "least", "mogul",
                "fetch", "cause", "oaken", "aglow", "meaty", "gaffe", "shyly", "racer", "prowl", "thief", "stern",
                "poesy", "rocky", "tweet", "waist", "spire", "grope", "havoc", "patsy", "truly", "forty", "deity",
                "uncle", "swish", "giver", "preen", "bevel", "lemur", "draft", "slope", "annoy", "lingo", "bleak",
                "ditty", "curly", "cedar", "dirge", "grown", "horde", "drool", "shuck", "crypt", "cumin", "stock",
                "gravy", "locus", "wider", "breed", "quite", "chafe", "cache", "blimp", "deign", "fiend", "logic",
                "cheap", "elide", "rigid", "false", "renal", "pence", "rowdy", "shoot", "blaze", "envoy", "posse",
                "brief", "never", "abort", "mouse", "mucky", "sulky", "fiery", "media", "trunk", "yeast", "clear",
                "skunk", "scalp", "bitty", "cider", "koala", "duvet", "segue", "creme", "super", "grill", "after",
                "owner", "ember", "reach", "nobly", "empty", "speed", "gipsy", "recur", "smock", "dread", "merge",
                "burst", "kappa", "amity", "shaky", "hover", "carol", "snort", "synod", "faint", "haunt", "flour",
                "chair", "detox", "shrew", "tense", "plied", "quark", "burly", "novel", "waxen", "stoic", "jerky",
                "blitz", "beefy", "lyric", "hussy", "towel", "quilt", "below", "bingo", "wispy", "brash", "scone",
                "toast", "easel", "saucy", "value", "spice", "honor", "route", "sharp", "bawdy", "radii", "skull",
                "phony", "issue", "lager", "swell", "urine", "gassy", "trial", "flora", "upper", "latch", "wight",
                "brick", "retry", "holly", "decal", "grass", "shack", "dogma", "mover", "defer", "sober", "optic",
                "crier", "vying", "nomad", "flute", "hippo", "shark", "drier", "obese", "bugle", "tawny", "chalk",
                "feast", "ruddy", "pedal", "scarf", "cruel", "bleat", "tidal", "slush", "semen", "windy", "dusty",
                "sally", "igloo", "nerdy", "jewel", "shone", "whale", "hymen", "abuse", "fugue", "elbow", "crumb",
                "pansy", "welsh", "syrup", "terse", "suave", "gamut", "swung", "drake", "freed", "afire", "shirt",
                "grout", "oddly", "tithe", "plaid", "dummy", "broom", "blind", "torch", "enemy", "again", "tying",
                "pesky", "alter", "gazer", "noble", "ethos", "bride", "extol", "decor", "hobby", "beast", "idiom",
                "utter", "these", "sixth", "alarm", "erase", "elegy", "spunk", "piper", "scaly", "scold", "hefty",
                "chick", "sooty", "canal", "whiny", "slash", "quake", "joint", "swept", "prude", "heavy", "wield",
                "femme", "lasso", "maize", "shale", "screw", "spree", "smoky", "whiff", "scent", "glade", "spent",
                "prism", "stoke", "riper", "orbit", "cocoa", "guilt", "humus", "shush", "table", "smirk", "wrong",
                "noisy", "alert", "shiny", "elate", "resin", "whole", "hunch", "pixel", "polar", "hotel", "sword",
                "cleat", "mango", "rumba", "puffy", "filly", "billy", "leash", "clout", "dance", "ovate", "facet",
                "chili", "paint", "liner", "curio", "salty", "audio", "snake", "fable", "cloak", "navel", "spurt",
                "pesto", "balmy", "flash", "unwed", "early", "churn", "weedy", "stump", "lease", "witty", "wimpy",
                "spoof", "saner", "blend", "salsa", "thick", "warty", "manic", "blare", "squib", "spoon", "probe",
                "crepe", "knack", "force", "debut", "order", "haste", "teeth", "agent", "widen", "icily", "slice",
                "ingot", "clash", "juror", "blood", "abode", "throw", "unity", "pivot", "slept", "troop", "spare",
                "sewer", "parse", "morph", "cacti", "tacky", "spool", "demon", "moody", "annex", "begin", "fuzzy",
                "patch", "water", "lumpy", "admin", "omega", "limit", "tabby", "macho", "aisle", "skiff", "basis",
                "plank", "verge", "botch", "crawl", "lousy", "slain", "cubic", "raise", "wrack", "guide", "foist",
                "cameo", "under", "actor", "revue", "fraud", "harpy", "scoop", "climb", "refer", "olden", "clerk",
                "debar", "tally", "ethic", "cairn", "tulle", "ghoul", "hilly", "crude", "apart", "scale", "older",
                "plain", "sperm", "briny", "abbot", "rerun", "quest", "crisp", "bound", "befit", "drawn", "suite",
                "itchy", "cheer", "bagel", "guess", "broad", "axiom", "chard", "caput", "leant", "harsh", "curse",
                "proud", "swing", "opine", "taste", "lupus", "gumbo", "miner", "green", "chasm", "lipid", "topic",
                "armor", "brush", "crane", "mural", "abled", "habit", "bossy", "maker", "dusky", "dizzy", "lithe",
                "brook", "jazzy", "fifty", "sense", "giant", "surly", "legal", "fatal", "flunk", "began", "prune",
                "small", "slant", "scoff", "torus", "ninny", "covey", "viper", "taken", "moral", "vogue", "owing",
                "token", "entry", "booth", "voter", "chide", "elfin", "ebony", "neigh", "minim", "melon", "kneed",
                "decoy", "voila", "ankle", "arrow", "mushy", "tribe", "cease", "eager", "birth", "graph", "odder",
                "terra", "weird", "tried", "clack", "color", "rough", "weigh", "uncut", "ladle", "strip", "craft",
                "minus", "dicey", "titan", "lucid", "vicar", "dress", "ditch", "gypsy", "pasta", "taffy", "flame",
                "swoop", "aloof", "sight", "broke", "teary", "chart", "sixty", "wordy", "sheer", "leper", "nosey",
                "bulge", "savor", "clamp", "funky", "foamy", "toxic", "brand", "plumb", "dingy", "butte", "drill",
                "tripe", "bicep", "tenor", "krill", "worse", "drama", "hyena", "think", "ratio", "cobra", "basil",
                "scrum", "bused", "phone", "court", "camel", "proof", "heard", "angel", "petal", "pouty", "throb",
                "maybe", "fetal", "sprig", "spine", "shout", "cadet", "macro", "dodgy", "satyr", "rarer", "binge",
                "trend", "nutty", "leapt", "amiss", "split", "myrrh", "width", "sonar", "tower", "baron", "fever",
                "waver", "spark", "belie", "sloop", "expel", "smote", "baler", "above", "north", "wafer", "scant",
                "frill", "awash", "snack", "scowl", "frail", "drift", "limbo", "fence", "motel", "ounce", "wreak",
                "revel", "talon", "prior", "knelt", "cello", "flake", "debug", "anode", "crime", "salve", "scout",
                "imbue", "pinky", "stave", "vague", "chock", "fight", "video", "stone", "teach", "cleft", "frost",
                "prawn", "booty", "twist", "apnea", "stiff", "plaza", "ledge", "tweak", "board", "grant", "medic",
                "bacon", "cable", "brawl", "slunk", "raspy", "forum", "drone", "women", "mucus", "boast", "toddy",
                "coven", "tumor", "truer", "wrath", "stall", "steam", "axial", "purer", "daily", "trail", "niche",
                "mealy", "juice", "nylon", "plump", "merry", "flail", "papal", "wheat", "berry", "cower", "erect",
                "brute", "leggy", "snipe", "sinew", "skier", "penny", "jumpy", "rally", "umbra", "scary", "modem",
                "gross", "avian", "greed", "satin", "tonic", "parka", "sniff", "livid", "stark", "trump", "giddy",
                "reuse", "taboo", "avoid", "quote", "devil", "liken", "gloss", "gayer", "beret", "noise", "gland",
                "dealt", "sling", "rumor", "opera", "thigh", "tonga", "flare", "wound", "white", "bulky", "etude",
                "horse", "circa", "paddy", "inbox", "fizzy", "grain", "exert", "surge", "gleam", "belle", "salvo",
                "crush", "fruit", "sappy", "taker", "tract", "ovine", "spiky", "frank", "reedy", "filth", "spasm",
                "heave", "mambo", "right", "clank", "trust", "lumen", "borne", "spook", "sauce", "amber", "lathe",
                "carat", "corer", "dirty", "slyly", "affix", "alloy", "taint", "sheep", "kinky", "wooly", "mauve",
                "flung", "yacht", "fried", "quail", "brunt", "grimy", "curvy", "cagey", "rinse", "deuce", "state",
                "grasp", "milky", "bison", "graft", "sandy", "baste", "flask", "hedge", "girly", "swash", "boney",
                "coupe", "endow", "abhor", "welch", "blade", "tight", "geese", "miser", "mirth", "cloud", "cabal",
                "leech", "close", "tenth", "pecan", "droit", "grail", "clone", "guise", "ralph", "tango", "biddy",
                "smith", "mower", "payee", "serif", "drape", "fifth", "spank", "glaze", "allot", "truck", "kayak",
                "virus", "testy", "tepee", "fully", "zonal", "metro", "curry", "grand", "banjo", "axion", "bezel",
                "occur", "chain", "nasal", "gooey", "filer", "brace", "allay", "pubic", "raven", "plead", "gnash",
                "flaky", "munch", "dully", "eking", "thing", "slink", "hurry", "theft", "shorn", "pygmy", "ranch",
                "wring", "lemon", "shore", "mamma", "froze", "newer", "style", "moose", "antic", "drown", "vegan",
                "chess", "guppy", "union", "lever", "lorry", "image", "cabby", "druid", "exact", "truth", "dopey",
                "spear", "cried", "chime", "crony", "stunk", "timid", "batch", "gauge", "rotor", "crack", "curve",
                "latte", "witch", "bunch", "repel", "anvil", "soapy", "meter", "broth", "madly", "dried", "scene",
                "known", "magma", "roost", "woman", "thong", "punch", "pasty", "downy", "knead", "whirl", "rapid",
                "clang", "anger", "drive", "goofy", "email", "music", "stuff", "bleep", "rider", "mecca", "folio",
                "setup", "verso", "quash", "fauna", "gummy", "happy", "newly", "fussy", "relic", "guava", "ratty",
                "fudge", "femur", "chirp", "forte", "alibi", "whine", "petty", "golly", "plait", "fleck", "felon",
                "gourd", "brown", "thrum", "ficus", "stash", "decry", "wiser", "junta", "visor", "daunt", "scree",
                "impel", "await", "press", "whose", "turbo", "stoop", "speak", "mangy", "eying", "inlet", "crone",
                "pulse", "mossy", "staid", "hence", "pinch", "teddy", "sully", "snore", "ripen", "snowy", "attic",
                "going", "leach", "mouth", "hound", "clump", "tonal", "bigot", "peril", "piece", "blame", "haute",
                "spied", "undid", "intro", "basal", "shine", "gecko", "rodeo", "guard", "steer", "loamy", "scamp",
                "scram", "manly", "hello", "vaunt", "organ", "feral", "knock", "extra", "condo", "adapt", "willy",
                "polka", "rayon", "skirt", "faith", "torso", "match", "mercy", "tepid", "sleek", "riser", "twixt",
                "peace", "flush", "catty", "login", "eject", "roger", "rival", "untie", "refit", "aorta", "adult",
                "judge", "rower", "artsy", "rural", "shave", "bobby", "eclat", "fella", "gaily", "harry", "hasty",
                "hydro", "liege", "octal", "ombre", "payer", "sooth", "unset", "unlit", "vomit", "fanny"]


# defining splitting characters into an element into a list
def split(x):
    return [char for char in x]


# defining converting list to integer
def convert(y):
    this = [str(i) for i in y]
    answer = int("".join(this))
    return answer


print("----->THIS CAN FIND WORDLE WORDS FROM 03/28/2022 to 10/14/2027<-----")
question = input("Do you want to... \n 1. Find the word of a specific day \n 2. Find what day a word is\n")

# this checks to see what the user wants to do
while True:
    if question == "1":
        response = input("Please input date in mm/dd/yyyy\n")

        # making sure the users input is in the correct format
        while not len(response) == 10:
            print("Date must be in mm/dd/yyyy")
            response = input("Please input date in mm/dd/yyyy\n")

        if len(response) == 10:

            # getting index of user input for respected time and converting from list to number
            response = split(response)
            month = int(response[0]), int(response[1])
            month = convert(month)
            day = int(response[3]), int(response[4])
            day = convert(day)
            year = int(response[6]), int(response[7]), int(response[8]), int(response[9])
            year = convert(year)

            # calculating what day of the year it is and converting it to an index number which corresponds to wordle word
            s1 = floor(275 * month / 9)
            s2 = floor((month + 9) / 12)
            s3 = (1 + floor((year - 4 * floor(year / 4) + 2) / 3))
            s = s1 - (s2 * s3) + day - 30
            s = s - 87
            if year > 2022:
                s = s + (year - 2022)*365

            # this is accounting for the 2024 leap year
                if year > 2023:
                    s = s + 1
            print("The wordle word for that day is " + wordle_words[s])
            time.sleep(1.5)

            # asks user if they want to go again
            again = input("again? \n 1.Yes \n 2.No \n")
            if again == "2":
                break

    # this checks to see what the user wants to do
    if question == "2":
        word = input("What word do you want to find?\n")

        # checks if the users word is even in wordle
        while word not in wordle_words:
            print("The word " + word + " is not in wordle")
            word = input("What word do you want to find?\n")
        if word in wordle_words:
            # list of all of the months and their respective dates
            jan = 1
            feb = 2
            mar = 3
            apr = 4
            may = 5
            jun = 6
            jul = 7
            aug = 8
            sep = 9
            octo = 10
            nov = 11
            dec = 12
            year = 2022
            ans = wordle_words.index(word)
            index = ans

            # we add 28 because the first word is on 3/28/2022 which is index 0
            ans += 28

            # checking to see if the word is at the start of a different year and setting the date to that year
            if ans < 307:
                date = [mar, "/", ans, "/", year]
            elif 306 < ans < 673:
                ans -= 306
                year = 2023
                date = [jan, "/", ans, "/", year]
            elif 672 < ans < 1038:
                ans -= 673
                year = 2024
                date = [jan, "/", ans, "/", year]
            elif 1037 < ans < 1403:
                ans -= 1037
                year = 2025
                date = [jan, "/", ans, "/", year]
            elif 1402 < ans < 1767:
                ans -= 1402
                year = 2026
                date = [jan, "/", ans, "/", year]
            elif ans > 1767:
                ans -= 1767
                year = 2027
                date = [jan, "/", ans, "/", year]

            # slowly subtracts the index number by the amount of days in that month
            if date[0] == jan and date[2] > 31:
                date[0] = feb
                date[2] -= 31
            if date[0] == feb and date[2] > 28:
                date[0] = mar
                date[2] -= 28
            if date[0] == mar and date[2] > 31:
                date[0] = apr
                date[2] -= 31
            if date[0] == apr and date[2] > 30:
                date[0] = may
                date[2] -= 30
            if date[0] == may and date[2] > 31:
                date[0] = jun
                date[2] -= 31
            if date[0] == jun and date[2] > 30:
                date[0] = jul
                date[2] -= 30
            if date[0] == jul and date[2] > 31:
                date[0] = aug
                date[2] -= 31
            if date[0] == aug and date[2] > 31:
                date[0] = sep
                date[2] -= 31
            if date[0] == sep and date[2] > 30:
                date[0] = octo
                date[2] -= 30
            if date[0] == octo and date[2] > 31:
                date[0] = nov
                date[2] -= 31
            if date[0] == nov and date[2] > 30:
                date[0] = dec
                date[2] -= 30
            if date[0] == dec and date[2] > 31:
                date[0] = jan
                date[2] -= 31

            # gets the final date and puts it in a string so the user can read it better
            date = "".join(str(i) for i in date)
            print("The word " + wordle_words[index] + " will be on", date)
            time.sleep(1.5)

            # asks user if they want to go again
            again = input("again? \n 1.Yes \n 2.No \n")
            if again == "2":
                break
                
