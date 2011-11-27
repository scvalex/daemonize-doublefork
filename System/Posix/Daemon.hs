-- | This module provides 'startDaemon' and 'stopDaemon' to facilitate
-- the creation of daemon programs.
--
-- The problem is as follows: the user starts a program in their
-- terminal, but he wants the program to relinquish control of the
-- terminal immediately, and furthermore, the program (or part of it)
-- should keep running even after said terminal is closed.  Examples
-- of programs that behave like this are @nginx@ and @emacs --daemon@.
--
-- The correct solution is to double-fork a process.  This ensures
-- that the child process is completed separated from the terminal it
-- was started on.
--
module System.Posix.Daemon (
        -- * Daemon control
        startDaemon, stopDaemon,

        -- * Utilities
        becomeGroupUser
    ) where

import Control.Monad ( when )
import System.Directory ( doesFileExist )
import System.Exit ( ExitCode(..) )
import System.IO ( SeekMode(..) )
import System.Posix.Directory ( changeWorkingDirectory )
import System.Posix.Files ( setFileCreationMask
                          , unionFileModes, otherModes, groupWriteMode )
import System.Posix.IO ( openFd, OpenMode(..), defaultFileFlags, closeFd
                       , dupTo, stdInput, stdOutput, stdError, getLock
                       , LockRequest (..), createFile, setLock, fdWrite
                       )
import System.Posix.Process ( getProcessID
                            , forkProcess, exitImmediately, createSession )
import System.Posix.Signals ( installHandler, sigHUP, Handler(..)
                            , signalProcess )
import System.Posix.Types ( ProcessID )
import System.Posix.User ( groupID, setGroupID, getGroupEntryForName
                         , userID, setUserID, getUserEntryForName )

-- | Double-fork to create a well behaved daemon.  If PIDFILE is
-- given, check/set pidfile; if we cannot obtain a lock on the file,
-- another process is already using it, so fail.  The program is
-- started with @SIGHUP@ masked; HANDLER is invoked on @SIGHUP@.
--
-- See: <http://www.enderunix.org/docs/eng/daemon.php>
--
-- Note: All unnecessary fds should be close before calling this.
--
-- HANDLER is meant to allow the daemon to shutdown cleanly.  It could
-- simply be:
--
-- @
--   handler = return ()
-- @
--
-- or something more elaborate like the following, which allows one to
-- perform some actions before re-raising the signal and killing the
-- daemon:
--
-- @
--   handler = do
--     putStrLn "Stopping daemon..."
--     raiseSignal sigTERM
-- @
--
startDaemon :: FilePath         -- ^ PIDFILE
            -> IO ()            -- ^ HANDLER
            -> IO ()            -- ^ PROGRAM
            -> IO ()
startDaemon pidFile handler program = do
  checkRunning
  _ <- forkProcess p            -- fork first child...
  exitImmediately ExitSuccess   -- ...and exit
    where
      p  = do
        _ <- createSession          -- create a new session and make
                                    -- this process its leader; see
                                    -- setsid(2)
        _ <- forkProcess p'         -- fork second child...
        return ()
      p' = do
        remapFds                -- remap standard fds
        -- files created by server should be at most rwxr-x---
        _ <- setFileCreationMask $ unionFileModes otherModes groupWriteMode
        changeWorkingDirectory "/" -- chdir
        _ <- installHandler sigHUP (Catch handler) Nothing
        setRunning              -- lock file
        program                 -- run the daemon

      remapFds = do
        devnull <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
        mapM_ (dupTo devnull) [stdInput, stdOutput, stdError]
        closeFd devnull

      checkRunning = do
        fe <- doesFileExist pidFile
        when fe $ do
          fd <- openFd pidFile WriteOnly Nothing defaultFileFlags
          ml <- getLock fd (ReadLock, AbsoluteSeek, 0, 0)
          closeFd fd
          case ml of
            Just (pid, _) -> fail (show pid ++ " already running")
            Nothing       -> return ()

      setRunning = do
        fd <- createFile pidFile 777
        setLock fd (WriteLock, AbsoluteSeek, 0, 0)
        pid <- getProcessID
        _ <- fdWrite fd (show pid)
        return ()

-- | Stop the daemon identified by PIDFILE by sending it @SIGHUP@.  If
-- the process was daemonized with 'startDaemon', the handler
-- specified there will be invoked first.  Return the pid of the
-- process that was killed; if PIDFILE does not exist, return
-- 'Nothing'.
stopDaemon :: FilePath          -- ^ PIDFILE
           -> IO (Maybe ProcessID)
stopDaemon pidFile = do
  fe <- doesFileExist pidFile
  if fe
     then do
       pid <- return . read =<< readFile pidFile
       signalProcess sigHUP pid
       return (Just pid)
     else
       return Nothing

-- | Make the current process belong to USER and GROUP.
becomeGroupUser :: String       -- ^ GROUP
                -> String       -- ^ USER
                -> IO ()
becomeGroupUser group user = do
  getGroupEntryForName group >>= setGroupID . groupID
  getUserEntryForName user >>= setUserID . userID

