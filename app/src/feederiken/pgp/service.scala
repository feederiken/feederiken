package feederiken.pgp

import zio._

import java.io._
import java.util.Date
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
  def computeFpr(kp: DatedKeyPair): UIO[Chunk[Byte]]
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

  def computeFpr(kp: DatedKeyPair): UIO[Chunk[Byte]] = UIO {
    Chunk.fromArray(
      new JcaPGPKeyPair(PublicKeyAlgorithmTags.EDDSA, kp.kp, kp.creationTime).getPublicKey.getFingerprint
    )
  }

  final val hashAlgorithmTag = HashAlgorithmTags.SHA256
  def makeRing(kp: DatedKeyPair, userId: String): UIO[KeyRing] =
    for {
      checksumCalculator <- UIO {
        new JcaPGPDigestCalculatorProviderBuilder()
          .setProvider(provider)
          .build()
          .get(hashAlgorithmTag)
      }
      jcakp = new JcaPGPKeyPair(PublicKeyAlgorithmTags.EDDSA, kp.kp, kp.creationTime)
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
