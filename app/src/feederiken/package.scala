package object feederiken {
  type Env = zio.ZEnv with pgp.PGP with zio.logging.Logging
}
