This is a program written in Haskell (obviously).

The goal of this program will be to simulate a fight between two (or more) players.

> import System.Random (newStdGen, randomR)

First off, lets define a `Player`.

> data Player = Player {
>                        playerName :: String
>                      , playerHealth :: Int
>                      , playerMaxHealth :: Int
>                      , playerWeapon :: Weapon
>                      }
>             | Dead   {
>                        playerName :: String
>                      , playerHealth :: Int
>                      , playerMaxHealth :: Int
>                      , playerWeapon :: Weapon
>                      } deriving (Show)


Now, lets define a `Weapon`.

> data Weapon = Weapon {
>                        weaponName :: String
>                      , weaponMinDamage :: Int
>                      , weaponMaxDamage :: Int
>                      , weaponMetadata :: (String, Int, Int) -- (ActionString, Used, Durability)
>                      }
>             | Broken {
>                        weaponName :: String
>                      , weaponMinDamage :: Int
>                      , weaponMaxDamage :: Int
>                      , weaponMetadata :: (String, Int, Int) -- (ActionString, Used, Durability)
>                      }
>             deriving (Show)

Lets go into detail about `weaponMetadata`.
The first element of the tuple is the `String`. The `String` describes how the weapon is used.
For example,
    for a sword, the `String` could be "`strikes`"
    for a magical wand, it could be "`summons fireball`"
                        or "`summons a bolt of lightning`" or anything else.

The second element is the `Int`. The `Int` determines how many times the weapon has been used.
It should be 0 for a new weapon.
It should be 1 for a weapon that is used once. Et cetera.

The third element is another `Int`. It is the durability of the weapon,
ie. how many times the weapon was meant to be used. It can be ignored too.

Note that the metadata isn't meant to be included in the comparison of weapons.

A broken weapon could be due to it being used many many times or some other
unforseen circumstances.

When comparing a `Weapon` and a `Broken`, if
  - `weaponName`,
  - `weaponMinDamage` and
  - `weaponMaxDamage`
are equal, they are considered equal. They are only treated as different in terms
of how they are treated.

> instance Eq Weapon where
>         a == b = weaponName a == weaponName b
>                    && weaponMinDamage a == weaponMinDamage b
>                    && weaponMaxDamage a == weaponMaxDamage b

As for why there is a `weaponMinDamage` and `weaponMaxDamage`, I will explain
later.



Lets do the easiest function first, resting.
Resting should recover 8.5% (default) of max health. (randomly chosen)
You can configure it below.

*##############################################################################*
*                C O N F I G U R A T I O N   O P T I O N S                     *
*##############################################################################*

> _REST_INCREASE_PERCENTAGE = 8.5 -- Used by rest (calcNewHealth)
> _REST_TAKE_CEILING_OR_FLOOR = ceiling -- round up = ceiling; round down = floor

*##############################################################################*

Now to implement `rest`.

> rest :: Player -> IO Player
> rest a
>     = return $ if newHealth <= mHealth
>       then a { playerHealth = newHealth }
>       else a { playerHealth = mHealth }
>     where
>     health = playerHealth a
>     mHealth = playerMaxHealth a
>     newHealth = health + calcIncr mHealth
>     calcIncr :: Int -> Int
>     calcIncr a = _REST_TAKE_CEILING_OR_FLOOR $ fromIntegral a * (_REST_INCREASE_PERCENTAGE / 100)

Let's also write a `reduceHealth` function.
It takes an Int and subtracts it from the player's health
So if you pass it a negative Int, it actually adds health.
However, if the player is dead, we return Nothing as we can't reduce the health of a dead player.

> reduceHealth :: Player -> Int -> IO (Maybe Player)
> reduceHealth a@(Player {playerHealth = b}) c = if b - c < 0
>                                                then killPlayer a
>                                                else return $ Just a { playerHealth = b - c }
> reduceHealth (Dead {}) _ = return Nothing


If a player is dead, we return Nothing
Else, we turn the Player to Dead

> killPlayer :: Player -> IO (Maybe Player)
> killPlayer (Player { playerName = a, playerHealth = b, playerMaxHealth = c, playerWeapon = d }) = return $ Just $ Dead a b c d
> killPlayer (Dead {}) = return Nothing


`useWeapon` increments the weapon's use by 1.

> useWeapon :: Weapon -> IO Weapon
> useWeapon a@(Weapon { weaponMetadata = (b, c, d)}) = return $ a { weaponMetadata = (b, c + 1, d) }
> useWeapon a@(Broken { weaponMetadata = (b, c, d)}) = return $ a { weaponMetadata = (b, c + 1, d) }

If a weapon is already broken, we return Nothing
Else, we turn the Weapon into Broken

> breakWeapon :: Weapon -> IO (Maybe Weapon)
> breakWeapon (Weapon { weaponName = a, weaponMinDamage = b, weaponMaxDamage = c, weaponMetadata = d }) = return $ Just $ Broken a b c d
> breakWeapon (Broken {}) = return Nothing

`useWeapon'` increments the use by 1 using useWeapon.
If the weapon's use matches durabilty,
`useWeapon'` will break the weapon as well using breakWeapon.

> useWeapon' :: Weapon -> IO (Maybe Weapon)
> useWeapon' a = do
>                weapon <- useWeapon a
>                case weapon of
>                     Weapon { weaponMetadata = (a, use, durability)} -> if use == durability
>                                                                        then breakWeapon weapon
>                                                                        else return $ Just weapon
>                     Broken {} -> return Nothing


This function calculates an Int between weaponMinDamage and weaponMaxDamage

> calculateDamage :: Weapon -> IO Int
> calculateDamage (Weapon { weaponMinDamage = _min, weaponMaxDamage = _max })
>     = newStdGen >>= (return . fst . randomR (_min, _max))
> calculateDamage (Broken { weaponMinDamage = _min, weaponMaxDamage = _max })
>     = newStdGen >>= (return . fst . randomR (_min, _max))


*##############################################################################*
*                C O N F I G U R A T I O N   O P T I O N S                     *
*##############################################################################*

> _ATTACK_MISS_CHANCE = 20 -- Percentage; The chance that a player will miss his/her attack.

*##############################################################################*

Firstly, it "uses" the weapon of the attacker by using `useWeapon'`
As `useWeapon'` returns Nothing if the weapon is already broken,
we can safely assure that the weapon isn't broken before use if it returns Just.
After that, we see if the player misses.
If he/she does, we return the victim untouched and the player with his used weapon.
Else, we calculate the damage dealt by the attacker.
Then we check the victim. Was he already dead? If yes, we return Nothing.
We take health off of the victim and we are done!

> attack :: Player -> Player -> IO (Maybe (Int, Player, Player))
> a@(Player {}) `attack` b = do
>                let weaponOfA = playerWeapon a
>                _weap <- useWeapon' weaponOfA
>                _missThreshold <- missThreshold
>                if _missThreshold < _ATTACK_MISS_CHANCE
>                    then do
>                         a2 <- rest a
>                         return $ _weap >>= \x -> return (0, a2 { playerWeapon = x }, b)
>                    else case _weap of
>                              Just usedWeapon -> do -- Proceed. Weapon was not broken.
>                                                 dmgDealt <- calculateDamage weaponOfA
>                                                 hurtB <- reduceHealth b dmgDealt
>                                                 case hurtB of
>                                                      Just hurtedB -> return $ Just (dmgDealt, a { playerWeapon = usedWeapon }, hurtedB)
>                                                      Nothing -> return Nothing
>                              Nothing -> return Nothing
>                where
>                missThreshold :: IO Int
>                missThreshold = newStdGen >>= return . fst . randomR (0, 100)
> _ `attack` _ = return Nothing -- Dead players can't attack!

*##############################################################################*
*                C O N F I G U R A T I O N   O P T I O N S                     *
*##############################################################################*

> weapA = Weapon "Sword" 3 12 ("strikes", 0, 7)
> weapB = Weapon "tentacles" 4 11 ("squeezes", 0, 7)
> playerA = Player "Aether_knight" 40 40 weapA
> playerB = Player "Evelyn" 40 40 weapB

*##############################################################################*

Now for the main program!

> main :: IO ()
> main = do
>        putStrLn $ playerName playerA ++ " has entered the battle!"
>        putStrLn $ playerName playerB ++ " has entered the battle!"
>        putStrLn $ playerName playerA ++ " has " ++ show (playerHealth playerA) ++ "/" ++ show (playerMaxHealth playerA)  ++ " health."
>        putStrLn $ playerName playerB ++ " has " ++ show (playerHealth playerB) ++ "/" ++ show (playerMaxHealth playerB) ++ " health."
>        startLoop playerA playerB

> startLoop :: Player -> Player -> IO ()
> startLoop a@(Player {}) b@(Player {}) = do
>                                         (_a, _b) <- doAttack a b
>                                         startLoop _b _a
> startLoop a@(Dead {}) b@(Dead {})     = error "WEIRD. Both players are dead?!"
> startLoop             _ b@(Dead {})   = putStrLn $ playerName b ++ " is dead!"
> startLoop a@(Dead {})               _ = putStrLn $ playerName a ++ " is dead!"

> doAttack :: Player -> Player -> IO (Player, Player)
> doAttack a b = do
>                result <- a `attack` b
>                case result of
>                     Nothing -> do
>                                putStrLn $ playerName a ++ "'s weapon is broken and is unable to attack!'"
>                                deadA <- killPlayer a
>                                case deadA of
>                                     Just dead -> return (dead, b)
>                                     Nothing -> return (a, b)
>                     Just (dmg, _a, _b) -> do
>                                           let weap = playerWeapon a
>                                           let (attackStr, _, _) = weaponMetadata weap
>                                           if dmg == 0
>                                               then do
>                                                    putStrLn $ playerName _a ++ " misses!"
>                                                    putStrLn $ playerName _a ++ " rested and now has " ++ show (playerHealth _a) ++ " health!"
>                                               else putStrLn $ playerName _a ++ " " ++ attackStr ++ " with " ++ weaponName weap ++ " for " ++ show dmg ++ "!"
>                                           return (_a, _b)
