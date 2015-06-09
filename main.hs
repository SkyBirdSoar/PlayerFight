import System.Random (newStdGen, randomR)

_REST_INCREASE_PERCENTAGE = 8.5
_REST_TAKE_CEILING_OR_FLOOR = ceiling
_ATTACK_MISS_CHANCE = 20

weapA = Weapon "Sword" 3 12 ("strikes", 0, 7)
weapB = Weapon "tentacles" 4 11 ("squeezes", 0, 7)
playerA = Player "Aether_knight" 40 40 weapA
playerB = Player "Evelyn" 40 40 weapB

data Player = Player {
                       playerName :: String
                     , playerHealth :: Int
                     , playerMaxHealth :: Int
                     , playerWeapon :: Weapon
                     }
            | Dead   {
                       playerName :: String
                     , playerHealth :: Int
                     , playerMaxHealth :: Int
                     , playerWeapon :: Weapon
                     } deriving (Show)

data Weapon = Weapon {
                       weaponName :: String
                     , weaponMinDamage :: Int
                     , weaponMaxDamage :: Int
                     , weaponMetadata :: (String, Int, Int)
                     }
            | Broken {
                       weaponName :: String
                     , weaponMinDamage :: Int
                     , weaponMaxDamage :: Int
                     , weaponMetadata :: (String, Int, Int)
                     } deriving (Show)

instance Eq Weapon where
         a == b = weaponName a == weaponName b
                    && weaponMinDamage a == weaponMinDamage b
                    && weaponMaxDamage a == weaponMaxDamage b

rest :: Player -> IO Player
rest a
    = return $ if newHealth <= mHealth
      then a { playerHealth = newHealth }
      else a { playerHealth = mHealth }
    where
    health = playerHealth a
    mHealth = playerMaxHealth a
    newHealth = health + calcIncr mHealth
    calcIncr :: Int -> Int
    calcIncr a = _REST_TAKE_CEILING_OR_FLOOR $ fromIntegral a * (_REST_INCREASE_PERCENTAGE / 100)

reduceHealth :: Player -> Int -> IO (Maybe Player)
reduceHealth a@(Player {playerHealth = b}) c = if b - c < 0
                                               then killPlayer a
                                               else return $ Just a { playerHealth = b - c }
reduceHealth (Dead {}) _ = return Nothing

killPlayer :: Player -> IO (Maybe Player)
killPlayer (Player { playerName = a, playerHealth = b, playerMaxHealth = c, playerWeapon = d }) = return $ Just $ Dead a b c d
killPlayer (Dead {}) = return Nothing

useWeapon :: Weapon -> IO Weapon
useWeapon a@(Weapon { weaponMetadata = (b, c, d)}) = return $ a { weaponMetadata = (b, c + 1, d) }
useWeapon a@(Broken { weaponMetadata = (b, c, d)}) = return $ a { weaponMetadata = (b, c + 1, d) }

breakWeapon :: Weapon -> IO (Maybe Weapon)
breakWeapon (Weapon { weaponName = a, weaponMinDamage = b, weaponMaxDamage = c, weaponMetadata = d }) = return $ Just $ Broken a b c d
breakWeapon (Broken {}) = return Nothing

useWeapon' :: Weapon -> IO (Maybe Weapon)
useWeapon' a = do
               weapon <- useWeapon a
               case weapon of
                    Weapon { weaponMetadata = (a, use, durability)} -> if use == durability
                                                                       then breakWeapon weapon
                                                                       else return $ Just weapon
                    Broken {} -> return Nothing

calculateDamage :: Weapon -> IO Int
calculateDamage (Weapon { weaponMinDamage = _min, weaponMaxDamage = _max })
    = newStdGen >>= (return . fst . randomR (_min, _max))
calculateDamage (Broken { weaponMinDamage = _min, weaponMaxDamage = _max })
    = newStdGen >>= (return . fst . randomR (_min, _max))

attack :: Player -> Player -> IO (Maybe (Int, Player, Player))
a@(Player {}) `attack` b = do
               let weaponOfA = playerWeapon a
               _weap <- useWeapon' weaponOfA
               _missThreshold <- missThreshold
               if _missThreshold < _ATTACK_MISS_CHANCE
                   then do
                        a2 <- rest a
                        return $ _weap >>= \x -> return (0, a2 { playerWeapon = x }, b)
                   else case _weap of
                             Just usedWeapon -> do
                                                dmgDealt <- calculateDamage weaponOfA
                                                hurtB <- reduceHealth b dmgDealt
                                                case hurtB of
                                                     Just hurtedB -> return $ Just (dmgDealt, a { playerWeapon = usedWeapon }, hurtedB)
                                                     Nothing -> return Nothing
                             Nothing -> return Nothing
               where
               missThreshold :: IO Int
               missThreshold = newStdGen >>= return . fst . randomR (0, 100)
_ `attack` _ = return Nothing

main :: IO ()
main = do
       putStrLn $ playerName playerA ++ " has entered the battle!"
       putStrLn $ playerName playerB ++ " has entered the battle!"
       putStrLn $ playerName playerA ++ " has " ++ show (playerHealth playerA) ++ "/" ++ show (playerMaxHealth playerA)  ++ " health."
       putStrLn $ playerName playerB ++ " has " ++ show (playerHealth playerB) ++ "/" ++ show (playerMaxHealth playerB) ++ " health."
       startLoop playerA playerB

startLoop :: Player -> Player -> IO ()
startLoop a@(Player {}) b@(Player {}) = do
                                        (_a, _b) <- doAttack a b
                                        startLoop _b _a
startLoop a@(Dead {}) b@(Dead {})     = error "WEIRD. Both players are dead?!"
startLoop             _ b@(Dead {})   = putStrLn $ playerName b ++ " is dead!"
startLoop a@(Dead {})               _ = putStrLn $ playerName a ++ " is dead!"

doAttack :: Player -> Player -> IO (Player, Player)
doAttack a b = do
               result <- a `attack` b
               case result of
                    Nothing -> do
                               putStrLn $ playerName a ++ "'s weapon is broken and is unable to attack!'"
                               deadA <- killPlayer a
                               case deadA of
                                    Just dead -> return (dead, b)
                                    Nothing -> return (a, b)
                    Just (dmg, _a, _b) -> do
                                          let weap = playerWeapon a
                                          let (attackStr, _, _) = weaponMetadata weap
                                          if dmg == 0
                                              then do
                                                   putStrLn $ playerName _a ++ " misses!"
                                                   putStrLn $ playerName _a ++ " rested and now has " ++ show (playerHealth _a) ++ " health!"
                                              else putStrLn $ playerName _a ++ " " ++ attackStr ++ " with " ++ weaponName weap ++ " for " ++ show dmg ++ "!"
                                          return (_a, _b)
