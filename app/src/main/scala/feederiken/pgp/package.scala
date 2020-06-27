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

package feederiken

import zio._
import java.io._
import java.util.Date

package object pgp {
  type PGP = Has[Service]

  type KeyPairGenerator = java.security.KeyPairGenerator
  type KeyPair = java.security.KeyPair
  type DatedKeyPair = org.bouncycastle.openpgp.PGPKeyPair
  type KeyRing = org.bouncycastle.openpgp.PGPSecretKeyRing

  def keyPairGenerator: ZManaged[PGP, Nothing, KeyPairGenerator] =
    ZManaged.service[Service] >>= { _.keyPairGenerator }

  def genKeyPair(kpg: KeyPairGenerator) =
    ZIO.service[Service] >>= { _.genKeyPair(kpg) }

  def dateKeyPair(rawkp: KeyPair)(creationTime: Date) =
    ZIO.service[Service] >>= { _.dateKeyPair(rawkp)(creationTime) }

  def makeRing(kp: DatedKeyPair, userId: String) =
    ZIO.service[Service] >>= { _.makeRing(kp, userId) }

  def loadRing(in: InputStream) =
    ZIO.service[Service] >>= { _.loadRing(in) }

  def saveRing(kr: KeyRing, out: OutputStream) =
    ZIO.service[Service] >>= { _.saveRing(kr, out) }

}
