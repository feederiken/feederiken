// This is free and unencumbered software released into the public domain.
//
// Anyone is free to copy, modify, publish, use, compile, sell, or
// distribute this software, either in source code form or as a compiled
// binary, for any purpose, commercial or non-commercial, and by any
// means.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
// OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
// ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.

package feederiken.pgp

import zio._
import zio.stream._


import java.io._
import java.util.Date
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.bcpg._
import org.bouncycastle.openpgp._
import org.bouncycastle.openpgp.operator._
import org.bouncycastle.openpgp.operator.jcajce._

trait Service {
  def keyPairGenerator: UManaged[KeyPairGenerator]
  def genKeyPair(kpg: KeyPairGenerator): UIO[KeyPair]
  def dateKeyPair(rawkp: KeyPair)(creationTime: Date): UIO[DatedKeyPair]
  def makeRing(kp: DatedKeyPair, userId: String): UIO[KeyRing]
  def loadRing(in: InputStream): ZIO[blocking.Blocking, IOException, KeyRing]
  def saveRing(kr: KeyRing, out: OutputStream): ZIO[blocking.Blocking, IOException, Unit]
}

private class BouncyCastleService(provider: BouncyCastleProvider) extends Service {
  def keyPairGenerator: UManaged[KeyPairGenerator] =
    Managed.effectTotal(java.security.KeyPairGenerator.getInstance("ed25519", provider))

  def genKeyPair(kpg: KeyPairGenerator) =
    UIO(kpg.generateKeyPair())

  def dateKeyPair(rawkp: KeyPair)(creationTime: Date) =
    UIO(new JcaPGPKeyPair(PublicKeyAlgorithmTags.EDDSA, rawkp, creationTime))

  final val hashAlgorithmTag = HashAlgorithmTags.SHA256
  def makeRing(kp: DatedKeyPair, userId: String): UIO[KeyRing] = for {
    checksumCalculator <- UIO(new JcaPGPDigestCalculatorProviderBuilder().setProvider(provider).build().get(hashAlgorithmTag))
    certificationLevel = PGPSignature.POSITIVE_CERTIFICATION
    hashedPcks = null: PGPSignatureSubpacketVector
    unhashedPcks = null: PGPSignatureSubpacketVector
    keySignerBuilder = new JcaPGPContentSignerBuilder(kp.getPublicKey.getAlgorithm, hashAlgorithmTag).setProvider(provider)
    keyEncryptor = null: PBESecretKeyEncryptor
    ring <- UIO(new PGPKeyRingGenerator(
        certificationLevel, kp, userId, checksumCalculator, hashedPcks, unhashedPcks, keySignerBuilder, keyEncryptor
      ).generateSecretKeyRing())
  } yield ring

  def loadRing(in: InputStream) = blocking.effectBlockingIO {
    new PGPPublicKeyRing(PGPUtil.getDecoderStream(in), new JcaKeyFingerprintCalculator)
  }

  def saveRing(kr: KeyRing, out: OutputStream) =
    Managed.fromAutoCloseable(UIO(new ArmoredOutputStream(out))).use { out =>
      blocking.effectBlockingIO(kr.encode(out))
    }
}

object PGP {
  def bouncyCastle: TaskLayer[PGP] =
    ZLayer.fromEffect(IO(new BouncyCastleService(new BouncyCastleProvider)))
}
