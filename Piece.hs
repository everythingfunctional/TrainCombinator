type InputDir = Float
type TrackVector = (Float, Float, Float)
type OutputDir = Float
data Piece a = Piece InputDir TrackVector OutputDir deriving (Show)

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
