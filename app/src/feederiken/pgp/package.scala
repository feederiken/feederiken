package feederiken

import zio._
import java.io._
import java.util.Date

/** PGP operations. Not very generic, tailored to the specific use case of feederiken.
  */
package object pgp {
  type PGP = Has[Service]

  type KeyPairGenerator = java.security.KeyPairGenerator
  type KeyPair = java.security.KeyPair
  case class DatedKeyPair(kp: KeyPair, creationTime: Date)
  type KeyRing = org.bouncycastle.openpgp.PGPSecretKeyRing

  // Length of PGP fingerprints in bytes
  final val FingerprintLength = 20

  /** Resource required to generate a sequence of random keypairs.
    */
  def keyPairGenerator: URManaged[PGP, KeyPairGenerator] =
    ZManaged.service[Service] >>= { _.keyPairGenerator }

  /** Pull a fresh random Ed25519 keypair from the generator.
    */
  def genKeyPair(kpg: KeyPairGenerator): URIO[PGP, KeyPair] =
    ZIO.service[Service] >>= { _.genKeyPair(kpg) }

  /** Compute the fingerprint of a keypair.
    */
  def computeFpr(kp: DatedKeyPair): URIO[PGP, Chunk[Byte]] =
    ZIO.service[Service] >>= { _.computeFpr(kp) }

  /** Build a keyring from its primary key. This doesn't effect the PGP fingerprint.
    */
  def makeRing(kp: DatedKeyPair, userId: String): URIO[PGP, KeyRing] =
    ZIO.service[Service] >>= { _.makeRing(kp, userId) }

  /** Read a secret keyring from a byte stream.
    */
  def loadRing(
      in: InputStream
  ): ZIO[PGP with blocking.Blocking, IOException, KeyRing] =
    ZIO.service[Service] >>= { _.loadRing(in) }

  /** Dump a keyring in PGP armored format.
    */
  def saveRing(
      kr: KeyRing,
      out: OutputStream,
  ): ZIO[PGP with blocking.Blocking, IOException, Unit] =
    ZIO.service[Service] >>= { _.saveRing(kr, out) }

}
