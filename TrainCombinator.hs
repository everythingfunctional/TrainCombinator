{-# OPTIONS_GHC -O -fglasgow-exts #-}
import Data.List

type InputDir = Float
type TrackVector = (Float, Float, Float)
type OutputDir = Float
data Piece = Piece InputDir TrackVector OutputDir deriving (Show, Eq)
type Track = [Piece]

rotateToAlign (Piece _ _ out_dir1)
              (Piece in_dir2 (x_dir, y_dir, z_dir) out_dir2)
              = let rotation = within360 (out_dir1 - in_dir2) in
                    Piece out_dir1 (rotate (x_dir, y_dir, z_dir) rotation) (within360 $ out_dir2 + rotation)

within360 deg
    | deg < 0 = within360 $ deg + 360
    | deg >= 360 = within360 $ deg - 360
    | otherwise = deg

rotate (x, y, z) rotation = let rot = rotation * pi / 180 in
                                (((x * cos rot) - (y * sin rot)), ((x * sin rot) + (y * cos rot)), z)

appendPieces (Piece in_dir1 in_vec1 out_dir1)
             (Piece in_dir2 in_vec2 out_dir2)
             = let (Piece new_in_dir new_vec new_out_dir) = rotateToAlign (Piece in_dir1 in_vec1 out_dir1)
                                                                          (Piece in_dir2 in_vec2 out_dir2) in
                (Piece in_dir1 (addVecs in_vec1 new_vec) new_out_dir)

addVecs (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

flipPiece (Piece in_dir (x, y, z) out_dir) = let (rotated_x, rotated_y, rotated_z) = rotate (x, y, z) (-in_dir)
                                                 (unrotated_x, unrotated_y, unrotated_z) = rotate (rotated_x, (-rotated_y), rotated_z) in_dir
                                                 rotated_out_dir = out_dir - in_dir
                                                 flipped_out_dir = (-rotated_out_dir)
                                                 unrotated_out_dir = flipped_out_dir + in_dir in
                                            (Piece in_dir (unrotated_x, unrotated_y, unrotated_z) unrotated_out_dir)

allPossibleTracks (p:[]) = [p]:[flipPiece p]:[]
allPossibleTracks (p:ps) = let subset = allPossibleTracks ps in
                         subset ++ (concatMap (\x -> permutations (p:x)) subset) ++ (concatMap (\x -> permutations ((flipPiece p):x)) subset)

rotateTrack n (p:[]) = p:[]
rotateTrack n ps = bs ++ as where (as, bs) = splitAt n ps

allRotations p = p:iter 1 where
                    iter i
                        | i >= length p = []
                        | otherwise = (rotateTrack i p):(iter (i + 1))

tracksEqual t1 t2 = t1 `elem` (allRotations t2)

removeDuplicates (x:xs) = x : (removeDuplicates (filter (\y -> not (tracksEqual x y)) xs))
removeDuplicates [] = []

tolerance = 0.01

isCircuit t = ((abs x) < tolerance) && ((abs y) < tolerance) && ((abs z) < tolerance) && ((abs out_angle) < tolerance)
    where Piece in_angle (x, y, z) out_angle = (foldr appendPieces (Piece 0 (0, 0, 0) 0) t)

allValidTracks ps = (removeDuplicates (filter isCircuit (allPossibleTracks ps)))

corner = Piece 0 (14.1421356237, 5.85786437627, 0) 45
small_corner = Piece 0 (7.07106781187, 2.92893218813, 0) 45
--small_corner = Piece 0 (7.42462120246, 3.07537879754, 0) 45
mini_straight = Piece 0 (5, 0, 0) 0
small_straight = Piece 0 (10, 0, 0) 0
straight = Piece 0 (15, 0, 0) 0
long_straight = Piece 0 (20, 0, 0) 0
go_up = Piece 0 (20, 0, 6) 0
go_down = Piece 0 (20, 0, (-6)) 0

--inventory = (replicate 8 corner) ++ (replicate 2 small_corner) ++ (replicate 2 mini_straight) ++ (replicate 2 small_straight) ++ (replicate 4 straight) ++ (replicate 2 long_straight) ++ (replicate 1 go_up) ++ (replicate 1 go_down)
inventory = (replicate 8 corner)

main = do let tracks = allValidTracks inventory in
            putStrLn (unlines (map show tracks))
