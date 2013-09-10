import System.Posix.StatVFS
import System.Posix.IO

main :: IO ()
main = do
  fd <- openFd "/etc/fstab" ReadOnly Nothing defaultFileFlags
  fStatVFS fd >>= print
  statVFS "/dev" >>= print
