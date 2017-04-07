type InputDir = (Float, Float)
type TrackVector = (Float, Float, Float)
type OutputDir = (Float, Float)
data Piece a = Piece InputDir TrackVector OutputDir deriving (Show)

rotateToAlign (Piece _ _ (x, y))
              (Piece (x_in, y_in) (x_dir, y_dir, z_dir) (x_out, y_out))
              = Piece (x_in, y_in) (x_dir, y_dir, z_dir) (x_out, y_out)
