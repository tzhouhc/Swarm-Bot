// Ting's Swarm Bot
// Inspiration came from Starcraft 2's famed Zerg race.
// Some code adapted from those originally written by Dave G. Wonnacott.

// Current version has quite some nice features.
// For one, it beats the crap out of Reference bot.

// - Master-bot (The Overlord) analyzes its view, and assign a value to each
// of its eight neighboring blocks, calculated according to their distance
// to points of interests on the view.
// - collectors are essentially mini-master-bots that cannot reproduce
// and try to return to master-bot after some time.
// - missiles exist mainly to destroy things, but also collect
// resources when no enemy in sight.
// - builders randomly creates collectors or bombs while
// collecting resources themselves.
// - bombs are like missiles, but times out and self-detonates;
// they mainly serve to add chaos to the map.



import util.Random

class ControlFunction {
    val make_bot = true // This is the top-level control to decide to be make bot or not at all.
    var n = 0   // frenquency checker
    val rnd = new Random()

    def loggable(text: String) = text.map(c => if ("=,()|".contains(c)) '_' else c)

    val dirs = List(XY(-1,-1), XY( 0,-1), XY( 1,-1),
                    XY(-1, 0), XY( 0, 0), XY( 1, 0),
                    XY(-1, 1), XY( 0, 1), XY( 1, 1))
    val up   = XY( 0,-1)
    val down = XY( 0, 1)
    val left = XY(-1, 0)
    val right= XY( 1, 0)

    def randomDir(): XY = XY(rnd.nextInt(3) - 1, rnd.nextInt(3) - 1)
    def randomGoodDir(view: View, gen: Int): XY = {
        // print("HI!")
        val choices = view.availableDirs(gen)
        choices(rnd.nextInt(choices.length))
    }

    // Short hands for doing stuff
    def move(dir: XY): String = "Move(direction="+dir.x+":"+dir.y+")"
    def say(quote: String): String = "Say(text="+loggable(quote)+")"
    def log(quote: String): String = "Log(text="+loggable(quote)+")"
    def state(quote: String): String = "Status(text="+loggable(quote)+")"
    def draw(a: XY, b:XY): String = "DrawLine(from="+a.x+":"+a.y+",to="+b.x+":"+b.y+",color=#00FF00)"
    def spawn(dir: XY, energy: Int, settings: String): String = "Spawn(direction=" + dir.toString + ",energy=" + energy.toString + settings + ")"
    def detonate(rad: Int, sound: String): String = "Say(text=" + sound + ")|Explode(" + rad.toString + ")"

    def respond(input: String): String = {
        val (opcode, paramMap) = CommandParser(input)
        n += 1
        if( opcode == "React" ) {
            val generation = paramMap("generation").toInt
            val viewString = paramMap("view")
            val time = paramMap("time").toInt
            val view = View(viewString)
            val _map = view.map()
            // Master-bot moves:
            if (generation == 0) {
                val dirChoice = view.chooseDir(0, _map)
                var speech = ""
                if (rnd.nextInt(25) == 3) {
                    val actualSpeech = rnd.nextInt(7) match {
                        case 0 => "You found my socks?"
                        case 1 => "Hear me BURRPRPPRP"
                        case 2 => "There is no...ooh."
                        case 3 => "sudo rm -rf ~/*"
                        case 4 => "I say random stuff"
                        case 5 => "I.puke(aLot, @you)"
                        case 6 => "Just return Int 42"
                    }
                    speech = speech + actualSpeech
                }
                if (make_bot && paramMap("energy").toInt >= 2500 && view.threatened(10, 300) && rnd.nextInt(5) == 0) {
                    move(dirChoice) + "|" + spawn(randomGoodDir(view, 0), 100, ",role=missile")
                }
                else if ( make_bot && paramMap("energy").toInt >= 500 && rnd.nextInt(10) == 0) {
                    move(dirChoice) + "|" + spawn(randomGoodDir(view, 0), 100, ",role=collector,born=" + time.toString)
                }
                else if (make_bot && paramMap("energy").toInt >= 4000 && rnd.nextInt(25) == 0) {
                    move(dirChoice) + "|" + spawn(randomGoodDir(view, 0), 800, ",role=builder,born=" + time.toString)
                }
                else move(dirChoice) + "|" + say(speech)
            }
            // Minibot moves.
            else {
                val role = paramMap("role")
                if (role == "collector") {
                    // collector strategies:
                    // once spawned, they will seek out for resources.
                    // when they have gathered enough credits or been at it for long enough,
                    // they seek to return to the masterbot.
                    // if they are very close to enemy bots or beasts,
                    // they blow up rather than being harassed.
                    // Update: Decrustation
                    // collectors try to salvage much of its resource by creating a
                    // missile before self-detonation
                    val born = paramMap("born")
                    if (time - born.toInt < 100 && paramMap("energy").toInt < 1250) {
                        if (view.threatened(1, 400)  && paramMap("energy").toInt > 300) {
                            val dirChoice = view.chooseDir(1, _map)
                            return spawn(dirChoice, paramMap("energy").toInt - 150, ",role=collector,born=" + time.toString) + "|" + detonate(6, "KABOOM!!!")
                        }
                        else if (view.threatened(1, 400)) {
                            return detonate(6, "KABOOM!!!")
                        }
                        else {
                            val dirChoice = view.chooseDir(1, _map)
                            move(dirChoice)
                        }
                    }
                    else {  // return to mom
                        if (view.threatened(1, 700)) {
                            if (paramMap("energy").toInt > 300) {
                                val dirChoice = view.chooseDir(1, _map)
                                return spawn(dirChoice, paramMap("energy").toInt - 150, ",role=collector,born=" + time.toString) + "|" + detonate(6, "KABOOM!!!")
                            }
                            else {
                                return detonate(6, "KABOOM!!!")
                            }
                        }
                        else {
                            val masterPos = paramMap("master").split(':')
                            val master = XY(masterPos(0).toInt, masterPos(1).toInt)
                            // print(master)
                            val _map2 = _map :+ Tuple2(master, 'M'):+ Tuple2(master, 'M'):+ Tuple2(master, 'M')
                            // print(_map(0) + "\n")
                            val dirChoice = view.chooseDir(3, _map2)
                            move(dirChoice)
                        }
                    }
                }
                else if (role == "missile") {
                    // missile strategies:
                    // they first check if there are anything worth blowing up
                    // around them. If not, they go on like collectors but with small
                    // incentives for resources but great desire to hug hostiles.
                    // if there is, they blow up violently.
                    // Update: Splitter
                    // missiles try to create a missile spawn before BOOMing.
                    var spawnstr = ""
                    if (view.threatened(3, 400) && paramMap("energy").toInt > 300) {
                        spawnstr = spawnstr + spawn(randomGoodDir(view, 1), paramMap("energy").toInt/2, "role=missile") + "|"
                    }
                    if (view.threatened(2, 400)) {
                        return spawnstr + detonate(6, "SPLASH!")
                    }
                    val dirChoice = view.chooseDir(2, _map)
                    spawnstr + move(dirChoice)
                }
                else if (role == "builder") {
                    // builder strategies:
                    // They wonder around like collectors, but if they have enough energy,
                    // they will randomly spawn collectors, missiles or bombs.
                    // if they are immediately threatened, they will also blow.
                    // Update: Decrustation
                    val dirChoice = view.chooseDir(1, _map)
                    val born = paramMap("born")
                    if (view.threatened(1, 400)) {
                        if (paramMap("energy").toInt > 300) {
                            return spawn(dirChoice, paramMap("energy").toInt - 150, ",role=builder,born=" + time.toString) + "|" + detonate(6, "KABOOM!!!")
                        }
                        else {
                            return detonate(6, "KABOOM!!!")
                        }
                    }
                    else if (time - born.toInt > 500 || paramMap("energy").toInt > 8000) {
                        move(dirChoice) + "|Set(role=collector)"
                    }
                    else if ( paramMap("energy").toInt >= 300 && rnd.nextInt(12) == 0) {
                        rnd.nextInt(3) match {
                            case 0 => move(dirChoice) + "|" + spawn(randomGoodDir(view, 1), 100, ",role=bomb,born=" + time.toString)
                            case 1 => move(dirChoice) + "|" + spawn(randomGoodDir(view, 1), 100, ",role=missile")
                            case 2 => move(dirChoice) + "|" + spawn(randomGoodDir(view, 1), 100, ",role=collector,born=" + time.toString)
                        }
                    }
                    else move(dirChoice)
                }
                else if (role == "bomb") {
                    // bombs live a timed life, seeking out hostiles in
                    // their 50 cycles of life. When they see no enemy, they gather
                    // resource to make their boom louder.
                    // Update: Splitter
                    val born = paramMap("born")
                    if (time - born.toInt < 50) {
                        var spawnstr = ""
                        if (view.threatened(3, 400) && paramMap("energy").toInt > 300) {
                            spawnstr = spawnstr + spawn(randomGoodDir(view, 1), paramMap("energy").toInt/2, "role=bomb") + "|"
                        }
                        if (view.threatened(2, 400)) {
                            return spawnstr + detonate(6, "SPLASH!")
                        }
                        else {
                            val dirChoice = view.chooseDir(2, _map)
                            move(dirChoice)
                        }
                    }
                    else {
                        detonate(9, "KABOOM!")
                    }
                }
                else {
                    val dirChoice = view.chooseDir(1, _map)
                    move(dirChoice) //+ state(role.toString)
                }
            }
        } else ""
    }
}

case class View(cells: String) {
    val size = math.sqrt(cells.length).intValue
    def center = XY(size/2, size/2)
    def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size
    def apply(absPos: XY) = cells.charAt(indexFromAbsPos(absPos))
    def touch(dir: XY)  = apply(XY(dir.x+size/2, dir.y+size/2))
    def absToRel(absPos: XY): XY = absPos - center
    def relToAbs(relPos: XY): XY = relPos + center

    def availableDirs(gen: Int): Array[XY] = {
        var result = Array[XY]()
        var good = gen match {
            case 0 => "BP_S"
            case _ => "BP_M"
        }
        for (i <- -1 to 1) {
            for (j <- -1 to 1; if !(i == j && j == 0)) {
                if ( good.contains(apply(relToAbs(XY(i, j))))) {
                    result = result :+ XY(i, j)
                }
            }
        }
        result  // All spots that are good and edible.
    }

    def map(): Array[Tuple2[XY, Char]] = {
        var result = Array[Tuple2[XY, Char]]()
        for (i <- 0 to size - 1) {
            for (j <- 0 to size - 1; if !(i == j && j == size/2)) {
                val found = apply(XY(i, j))
                if ( List('P', 'p', 'B', 'b', 'W', 'S', 's', 'M', 'm').contains(found)) {
                    result = result :+ Tuple2(absToRel(XY(i, j)), found)
                }
            }
        }
        result
    }

    // Special case proximity checker for bombs
    def proximity(range: Int): List[XY] = { // OK
        var result = List[XY]()
        for (i <- size/2 - range to size/2 + range) {
            for (j <- size/2 - range to size/2 + range; if !(i == j && j == size/2)) {
                if ( List('b', 's', 'm').contains(apply(XY(i, j)))) {
                    result = result :+ XY(i, j)
                }
            }
        }
        result
    }

    def dist(a: XY, b:XY): Int = {
        (math.pow((a.x - b.x), 2) + math.pow((a.y - b.y), 2)).toInt
    }

    def stepDist(a:XY, b:XY): Int = {
        math.max(math.abs(a.x - b.x), math.abs(a.y-b.y))
    }

    def find_angle(v1: XY, v2:XY): Double = {   // OK
        val num = (v1.x * v2.x + v1.y * v2.y + 0.0)
        val denom = math.sqrt(v1.x * v1.x + v1.y * v1.y) * math.sqrt(v2.x * v2.x + v2.y * v2.y)
        math.acos((num/denom).toDouble)
    }

    def threatened(range: Int, threshold: Int): Boolean = {
        val _map = proximity(range)
        var danger_level = 0.0
        for (i <- 0 to _map.length - 1) {
            val poi = _map(i)       // where is it
            val distance = stepDist(poi, center).toFloat + 0.1
            val how_bad = apply(poi) match {
                case 'b' => 500.0       // clashing with a beast costs 150
                case 's' => 1500.0      // clashing another minibot kills both
                case 'm' => 3000.0      // clashing a bigbot makes me disappear outright
                case _ => 0.0
            }
            danger_level += (how_bad / distance)
        }
        danger_level >= threshold
    }

    def chooseDir(gen: Int, _map: Array[Tuple2[XY, Char]]): XY = {
        // There is a trade-off here:
        // Need to recalculate every time for good dirs,
        // but this saves calculation on bad dirs that are not gonna be
        // good anyway, so I'm keeping it.
        val dirs = availableDirs(gen)
        var values = Array.fill(dirs.length){0.toFloat}
        for (i <- 0 to _map.length - 1) {
            val poi= _map(i)._1       // where is it; relative term
            val item = _map(i)._2   // what's good there
            //print(item)
            val item_value = item match {
                    // cases: 0 is master, 1 is harvesting collector, 3 is returning collector,
                    // 2 is missile
                    // 4 is bomb
                    case 'P' => gen match {
                        case 2 => Array(10, 1)
                        case 4 => Array(-10, 1)
                        case _ => Array(500, 1)
                    }
                    // case 'p' => Array(-300, 3)      // always avoid toxifera
                    case 'p' => Array(0, 1)
                    case 'B' => gen match {
                        case 2 => Array(25, 1)
                        case 4 => Array(-10, 1)
                        case _ => Array(800, 1)
                    }
                    case 'b' => gen match {
                        case 2 => Array(500, 2)
                        case 4 => Array(500, 2)
                        case _ => Array(-500, 2)
                    }
                    case 'W' => Array(-300, 3)
                    // case 'W' => Array(0, 1)
                    case 'S' => gen match {
                        case 0 => Array(25, 4)      // master WANT TO EAT collector
                        case _ => Array(-300, 3)    // collectors don't want to bump
                    }
                    case 'M' => gen match {
                        case 0 => Array(1, 1)
                        case 3 => Array(5000, 2)    // Returning collectors
                        case _ => Array(-1000, 2)
                    }
                    case 's' => gen match {
                        case 2 => Array(700, 2)
                        case 4 => Array(700, 2)
                        case _ => Array(-700, 2)    // civilians shall avoid enemy minibots
                    }
                    case 'm' => gen match {
                        case 2 => Array(1000, 2)
                        case 4 => Array(1000, 2)
                        case _ => Array(-1000, 2)   // civilians shall avoid enemy masterbots
                    }
                    // case '?' => Array(-100, 2)
            }
            for (j <- 0 to dirs.length - 1) {
                val choice = dirs(j)
                val distance = dist(poi, choice)    // both relative here
                values(j) += (item_value(0) / math.pow(distance, item_value(1))).toFloat
            }
        }
        val max_index = values.indexOf(values.max)
        dirs(max_index)
    }

}

case class XY(x: Int, y: Int) {
    // Some built-in function overloading here!
    // Vector addition! WOOO!
    def +(offset: XY) = XY(x+offset.x, y+offset.y)
    def -(offset: XY) = XY(x-offset.x, y-offset.y)
    def reverse() = XY(-1 * x, -1 * y)
    def approximate(): XY = {
        val max = math.max(math.abs(x), math.abs(x))
        XY(math.round(x/max), math.round(y/max))
    }
    // def to_dir(vector: XY) = no longer needed.
    override def toString(): String = x + ":" + y
}

object CommandParser {
    def apply(command: String) = {
        def splitParam(param: String) = {
            val segments = param.split('=')
            if( segments.length != 2 )
                throw new IllegalStateException("invalid key/value pair: " + param)
            (segments(0),segments(1))
        }

        val segments = command.split('(')
        if( segments.length != 2 )
            throw new IllegalStateException("invalid command: " + command)

        val params = segments(1).dropRight(1).split(',')
        val keyValuePairs = params.map( splitParam ).toMap
        (segments(0), keyValuePairs)
    }
}

class ControlFunctionFactory {
    def create = new ControlFunction().respond _
}
