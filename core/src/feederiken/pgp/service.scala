package feederiken.pgp

import zio._

import java.io._
import java.util.Date
import java.security.MessageDigest
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.bcpg._
import org.bouncycastle.openpgp._
import org.bouncycastle.openpgp.operator._
import org.bouncycastle.openpgp.operator.jcajce._

/** @see pgp
  */
trait Service {
  def keyPairGenerator: UManaged[KeyPairGenerator]
  def genKeyPair(kpg: KeyPairGenerator): UIO[KeyPair]
  def dateKeyPair(kp: KeyPair, creationTime: Date): UIO[DatedKeyPair]
  def makeRing(kp: DatedKeyPair, userId: String): UIO[KeyRing]
  def loadRing(in: InputStream): ZIO[blocking.Blocking, IOException, KeyRing]
  def saveRing(
      kr: KeyRing,
      out: OutputStream,
  ): ZIO[blocking.Blocking, IOException, Unit]
}

private class BouncyCastleService(provider: BouncyCastleProvider)
    extends Service {
  def keyPairGenerator: UManaged[KeyPairGenerator] =
    Managed.effectTotal {
      java.security.KeyPairGenerator.getInstance("ed25519", provider)
    }

  def genKeyPair(kpg: KeyPairGenerator) =
    UIO(kpg.generateKeyPair())

  private def makeJca(kp: KeyPair, creationTime: Date) =
    UIO(new JcaPGPKeyPair(PublicKeyAlgorithmTags.EDDSA, kp, creationTime))

  private val Ed25519Oid = Chunk(
    // per https://tools.ietf.org/html/draft-koch-eddsa-for-openpgp-04#section-6
    0x2b, 0x06, 0x01, 0x04, 0x01, 0xda, 0x47, 0x0f, 0x01,
  ).map(_.toByte)

  def sha1(data: Chunk[Byte]): UIO[Chunk[Byte]] =
    for {
      md <- UIO(MessageDigest.getInstance("SHA1", provider))
      hash <- UIO(md.digest(data.toArray))
    } yield Chunk.fromArray(hash)

  def dateKeyPair(kp: KeyPair, creationTime: Date): UIO[DatedKeyPair] =
    for {
      encoded <- UIO(kp.getPublic.getEncoded)
      q = Chunk.fromArray(encoded).drop(12)
      timestamp = creationTime.getTime() / 1000
      version = 4.toByte
      pkp =
        // per https://tools.ietf.org/html/rfc4880#page-42
        version +: (timestamp >> 24).toByte +: (timestamp >> 16).toByte +: (timestamp >> 8).toByte +: timestamp.toByte +:
          // per https://tools.ietf.org/html/draft-koch-eddsa-for-openpgp-04#section-4
          PublicKeyAlgorithmTags.EDDSA.toByte +: Ed25519Oid.length.toByte +: Ed25519Oid ++:
          // per https://tools.ietf.org/html/draft-koch-eddsa-for-openpgp-04#section-3
          1.toByte +: 7.toByte +: 0x40.toByte +: q

      fpr <-
        // per https://tools.ietf.org/html/rfc4880#section-12.2
        sha1(
          0x99.toByte +: (pkp.length >> 8).toByte +: pkp.length.toByte +: pkp
        )
    } yield DatedKeyPair(kp, creationTime, fpr)

  final val hashAlgorithmTag = HashAlgorithmTags.SHA256
  def makeRing(kp: DatedKeyPair, userId: String): UIO[KeyRing] =
    for {
      checksumCalculator <- UIO {
        new JcaPGPDigestCalculatorProviderBuilder()
          .setProvider(provider)
          .build()
          .get(hashAlgorithmTag)
      }
      jcakp <- makeJca(kp.kp, kp.creationTime)
      certificationLevel = PGPSignature.POSITIVE_CERTIFICATION
      hashedPcks = null: PGPSignatureSubpacketVector
      unhashedPcks = null: PGPSignatureSubpacketVector
      keySignerBuilder = new JcaPGPContentSignerBuilder(
        jcakp.getPublicKey.getAlgorithm,
        hashAlgorithmTag,
      ).setProvider(provider)
      keyEncryptor = null: PBESecretKeyEncryptor
      ring <- UIO {
        new PGPKeyRingGenerator(
          certificationLevel,
          jcakp,
          userId,
          checksumCalculator,
          hashedPcks,
          unhashedPcks,
          keySignerBuilder,
          keyEncryptor,
        ).generateSecretKeyRing()
      }
    } yield ring

  def loadRing(in: InputStream) =
    blocking.effectBlockingIO {
      new PGPSecretKeyRing(
        PGPUtil.getDecoderStream(in),
        new JcaKeyFingerprintCalculator,
      )
    }

  def saveRing(kr: KeyRing, out: OutputStream) =
    Managed.fromAutoCloseable(UIO(new ArmoredOutputStream(out))).use { out =>
      blocking.effectBlockingIO(kr.encode(out))
    }
}

object PGP {

  /** Provide a [pgp] layer using bouncycastle.
    */
  def bouncyCastle: TaskLayer[PGP] =
    ZLayer.fromEffect(IO(new BouncyCastleService(new BouncyCastleProvider)))
}
