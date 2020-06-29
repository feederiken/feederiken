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

/** PGP operations. Not very generic, tailored to the specific use case of feederiken.
 */
package object pgp {
  type PGP = Has[Service]

  type KeyPairGenerator = java.security.KeyPairGenerator
  type KeyPair = java.security.KeyPair
  type DatedKeyPair = org.bouncycastle.openpgp.PGPKeyPair
  type KeyRing = org.bouncycastle.openpgp.PGPSecretKeyRing

  /** Resource required to generate a sequence of random keypairs.
   */
  def keyPairGenerator: URManaged[PGP, KeyPairGenerator] =
    ZManaged.service[Service] >>= { _.keyPairGenerator }

  /** Pull a fresh random Ed25519 keypair from the generator.
   */
  def genKeyPair(kpg: KeyPairGenerator): URIO[PGP, KeyPair] =
    ZIO.service[Service] >>= { _.genKeyPair(kpg) }

  /** Combine a keypair and a timestamp. The PGP fingerprint depends on exactly both.
   */
  def dateKeyPair(rawkp: KeyPair)(creationTime: Date): URIO[PGP, DatedKeyPair] =
    ZIO.service[Service] >>= { _.dateKeyPair(rawkp)(creationTime) }

  /** Build a keyring from its primary key. This doesn't effect the PGP fingerprint.
   */
  def makeRing(kp: DatedKeyPair, userId: String): URIO[PGP, KeyRing] =
    ZIO.service[Service] >>= { _.makeRing(kp, userId) }

  /** Read a secret keyring from a byte stream.
   */
  def loadRing(in: InputStream): ZIO[PGP with blocking.Blocking, IOException, KeyRing] =
    ZIO.service[Service] >>= { _.loadRing(in) }

  /** Dump a keyring in PGP armored format.
   */
  def saveRing(kr: KeyRing, out: OutputStream): ZIO[PGP with blocking.Blocking, IOException, Unit] =
    ZIO.service[Service] >>= { _.saveRing(kr, out) }

}
