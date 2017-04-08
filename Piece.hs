type InputDir = Float
type TrackVector = (Float, Float, Float)
type OutputDir = Float
data Piece = Piece InputDir TrackVector OutputDir deriving (Show)

rotateToAlign (Piece _ _ out_dir1)
              (Piece in_dir2 (x_dir, y_dir, z_dir) out_dir2)
              = let rotation = within360 (out_dir1 - in_dir2) in
                    Piece out_dir1 (rotate (x_dir, y_dir, z_dir) rotation) (within360 $ out_dir2 + rotation)

within360 deg
    | deg < 0 = within360 $ deg + 360
    | deg > 360 = within360 $ deg - 360
    | otherwise = deg

rotate (x, y, z) rotation = let rot = rotation * pi / 180 in
                                (((x * cos rot) - (y * sin rot)), ((x * sin rot) + (y * cos rot)), z)

appendPieces (Piece in_dir1 in_vec1 out_dir1)
             (Piece in_dir2 in_vec2 out_dir2)
             = let (Piece new_in_dir new_vec new_out_dir) = rotateToAlign (Piece in_dir1 in_vec1 out_dir1)
                                                                          (Piece in_dir2 in_vec2 out_dir2) in
                (Piece in_dir1 (addVecs in_vec1 new_vec) new_out_dir)

addVecs (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
