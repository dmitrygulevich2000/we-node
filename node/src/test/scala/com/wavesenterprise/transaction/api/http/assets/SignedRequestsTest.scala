package com.wavesenterprise.transaction.api.http.assets

import com.wavesenterprise.api.http.assets._
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.transaction.Proofs
import com.wavesenterprise.transaction.smart.script.Script
import com.wavesenterprise.utils.Base58
import play.api.libs.json.Json
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SignedRequestsTest extends AnyFunSuite with Matchers {

  test("AssetIssueRequest json parsing works") {
    val json =
      """
        {
          "version": 2,
          "name": "string",
          "quantity": 100000,
          "timestamp": 1484064349669,
          "description": "string",
          "proofs": ["d3JTwzxHj3j74yemdvw2xAdFeMo6hDJ9i2s3v3e4GMrz2Q25G89Pp3HyXfKk3mwNcKWjtyoTWvTt1eLk2KzetoL"],
          "senderPublicKey": "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5",
          "decimals": 2,
          "reissuable": true,
          "fee": 100000,
          "script": null
        }
      """
    val req = Json.parse(json).validate[SignedIssueV2Request].get
    req.name shouldBe "string"
    req.quantity shouldBe 100000L
    req.fee shouldBe 100000L
    req.decimals shouldBe 2
    req.timestamp shouldBe 1484064349669L
    req.reissuable shouldBe true

    val tx = req.toTx.explicitGet()
    Base58.encode(tx.name) shouldBe "zVbyBrMk"
    Base58.encode(tx.description) shouldBe "zVbyBrMk"
    tx.reissuable shouldBe true
    tx.decimals shouldBe 2
    tx.fee shouldBe 100000L
    tx.quantity shouldBe 100000L
    tx.timestamp shouldBe 1484064349669L
    tx.proofs.proofs.head.base58 shouldBe "d3JTwzxHj3j74yemdvw2xAdFeMo6hDJ9i2s3v3e4GMrz2Q25G89Pp3HyXfKk3mwNcKWjtyoTWvTt1eLk2KzetoL"
  }

  test("AssetReissueRequest json parsing works") {
    val json =
      """
        |{
        |"version": 2,
        |"senderPublicKey":"D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5",
        |"assetId":"Ha35nwsnmYxHRF8UmKG3S523BycBLZFU4FZnjXryKd4L",
        |"quantity":100000,"reissuable":true,
        |"fee":100000,"timestamp":1234,
        |"reissuable":true,
        |"proofs":["4YWbtkDA7PHH1MCxEUaP12pkNRPNqpJh8X7aagZzLyDNbzgopXJb7NHNNV8rjXcy2WsAKX1wzti7Bishu8u6hwtF"]
        |}
      """.stripMargin
    val req = Json.parse(json).validate[SignedReissueV2Request].get
    req.assetId shouldBe "Ha35nwsnmYxHRF8UmKG3S523BycBLZFU4FZnjXryKd4L"
    req.proofs.head shouldBe "4YWbtkDA7PHH1MCxEUaP12pkNRPNqpJh8X7aagZzLyDNbzgopXJb7NHNNV8rjXcy2WsAKX1wzti7Bishu8u6hwtF"
    req.fee shouldBe 100000L
    req.quantity shouldBe 100000L
    req.timestamp shouldBe 1234L
    req.reissuable shouldBe true

    val tx = req.toTx.explicitGet()
    tx.assetId.base58 shouldBe "Ha35nwsnmYxHRF8UmKG3S523BycBLZFU4FZnjXryKd4L"
    tx.reissuable shouldBe true
    tx.fee shouldBe 100000L
    tx.quantity shouldBe 100000L
    tx.timestamp shouldBe 1234L
    tx.reissuable shouldBe true
    tx.proofs.proofs.head.base58 shouldBe "4YWbtkDA7PHH1MCxEUaP12pkNRPNqpJh8X7aagZzLyDNbzgopXJb7NHNNV8rjXcy2WsAKX1wzti7Bishu8u6hwtF"
  }

  test("AssetTransfer json parsing works") {
    val json =
      """
        |{
        |   "version": 2,
        |   "recipient":"3Myss6gmMckKYtka3cKCM563TBJofnxvfD7",
        |   "timestamp":1479462208828,
        |   "assetId":"GAXAj8T4pSjunDqpz6Q3bit4fJJN9PD4t8AK8JZVSa5u",
        |   "amount":100000,
        |   "fee":100000,
        |   "senderPublicKey":"D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5",
        |   "proofs":["4dPRTW6XyRQUTQwwpuZDCNy1UDHYG9WGsEQnn5v49Lj5uyh4XGDdwtEq3t6ZottweAXHieK32UokHwiTxGFtz9bQ"],
        |   "attachment":"A"
        |}
      """.stripMargin
    val req = Json.parse(json).validate[SignedTransferV2Request].get
    req.recipient shouldBe "3Myss6gmMckKYtka3cKCM563TBJofnxvfD7"
    req.timestamp shouldBe 1479462208828L
    req.assetId shouldBe Some("GAXAj8T4pSjunDqpz6Q3bit4fJJN9PD4t8AK8JZVSa5u")
    req.amount shouldBe 100000
    req.fee shouldBe 100000
    req.senderPublicKey shouldBe "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5"
    req.proofs.head shouldBe "4dPRTW6XyRQUTQwwpuZDCNy1UDHYG9WGsEQnn5v49Lj5uyh4XGDdwtEq3t6ZottweAXHieK32UokHwiTxGFtz9bQ"
    req.attachment shouldBe Some("A")

    val tx = req.toTx.explicitGet()
    Base58.encode(tx.sender.publicKey.getEncoded) shouldBe "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5"
    tx.timestamp shouldBe 1479462208828L
    tx.attachment shouldBe Base58.decode("A").get
    tx.assetId.get.base58 shouldBe "GAXAj8T4pSjunDqpz6Q3bit4fJJN9PD4t8AK8JZVSa5u"
    tx.amount shouldBe 100000
    tx.fee shouldBe 100000
    tx.proofs.proofs.head.base58 shouldBe "4dPRTW6XyRQUTQwwpuZDCNy1UDHYG9WGsEQnn5v49Lj5uyh4XGDdwtEq3t6ZottweAXHieK32UokHwiTxGFtz9bQ"
  }

  test("AssetTransfer with a fee in an asset json parsing works") {
    val json =
      """
        |{
        |   "version": 2,
        |   "senderPublicKey":"FJuErRxhV9JaFUwcYLabFK5ENvDRfyJbRz8FeVfYpBLn",
        |   "recipient":"3N9UuGeWuDt9NfWbC5oEACHyRoeEMApXAeq",
        |   "timestamp":1489054107569,
        |   "assetId":"6MPKrD5B7GrfbciHECg1MwdvRUhRETApgNZspreBJ8JL",
        |   "amount":1000,
        |   "fee":100,
        |   "feeAssetId":"6MPKrD5B7GrfbciHECg1MwdvRUhRETApgNZspreBJ8JL",
        |   "proofs":["UAhYXYdkFAFBuwAuUFP3yw7E8aRTyx56ZL4UPbT4ufomBzVLMRpdW2dCtJmfpCuPPMhGTvdzhXwb7o4ER6HAUpJ"],
        |   "attachment":"2Kk7Zsr1e9jsqSBM5hpF"
        |}
      """.stripMargin
    val req = Json.parse(json).validate[SignedTransferV2Request].get
    req.recipient shouldBe "3N9UuGeWuDt9NfWbC5oEACHyRoeEMApXAeq"
    req.timestamp shouldBe 1489054107569L
    req.assetId shouldBe Some("6MPKrD5B7GrfbciHECg1MwdvRUhRETApgNZspreBJ8JL")
    req.feeAssetId shouldBe Some("6MPKrD5B7GrfbciHECg1MwdvRUhRETApgNZspreBJ8JL")
    req.amount shouldBe 1000
    req.fee shouldBe 100
    req.senderPublicKey shouldBe "FJuErRxhV9JaFUwcYLabFK5ENvDRfyJbRz8FeVfYpBLn"
    req.proofs.head shouldBe "UAhYXYdkFAFBuwAuUFP3yw7E8aRTyx56ZL4UPbT4ufomBzVLMRpdW2dCtJmfpCuPPMhGTvdzhXwb7o4ER6HAUpJ"
    req.attachment shouldBe Some("2Kk7Zsr1e9jsqSBM5hpF")

    val tx = req.toTx.explicitGet()
    Base58.encode(tx.sender.publicKey.getEncoded) shouldBe "FJuErRxhV9JaFUwcYLabFK5ENvDRfyJbRz8FeVfYpBLn"
    tx.timestamp shouldBe 1489054107569L
    tx.attachment shouldBe Base58.decode("2Kk7Zsr1e9jsqSBM5hpF").get
    tx.assetId.get.base58 shouldBe "6MPKrD5B7GrfbciHECg1MwdvRUhRETApgNZspreBJ8JL"
    tx.feeAssetId.get.base58 shouldBe "6MPKrD5B7GrfbciHECg1MwdvRUhRETApgNZspreBJ8JL"
    tx.amount shouldBe 1000
    tx.fee shouldBe 100
    tx.proofs.proofs.head.base58 shouldBe "UAhYXYdkFAFBuwAuUFP3yw7E8aRTyx56ZL4UPbT4ufomBzVLMRpdW2dCtJmfpCuPPMhGTvdzhXwb7o4ER6HAUpJ"
  }

  test("AssetBurnRequest json parsing works") {
    val json =
      """
        |{
        |"version": 2,
        |"senderPublicKey":"D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5",
        |"assetId":"6eV67ffUPXVGktrmsoWv1ZRKTuKcWZjeCQXJjD26pTGS",
        |"quantity":10000,
        |"fee":100000000,"timestamp":1477302582842,
        |"proofs":["H3F8gAsKYeJAPmxCagLaCHycqkr8KiYvzJ4dhophZs31Unmg3dLwVK5k1v1M2Z5zLuQySthpf3DeEyhL6cdpbqp"]
        |}
      """.stripMargin
    val req = Json.parse(json).validate[SignedBurnV2Request].get
    req.senderPublicKey shouldBe "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5"
    req.proofs.head shouldBe "H3F8gAsKYeJAPmxCagLaCHycqkr8KiYvzJ4dhophZs31Unmg3dLwVK5k1v1M2Z5zLuQySthpf3DeEyhL6cdpbqp"
    req.fee shouldBe 100000000L
    req.quantity shouldBe 10000
  }

  test("SponsorFeeRequest json parsing works") {
    import com.wavesenterprise.api.http.assets.SponsorFeeRequest._

    val One = 100000000L
    val js1 = s"""{
  "type": 14,
  "id": "Gobt7AiyQAfduRkW8Mk3naWbzH67Zsv9rdmgRNmon1Mb",
  "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
  "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
  "fee": $One,
  "timestamp": 1520945679531,
  "proofs": [
   "3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7"
  ],
  "version": 1,
  "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
  "isEnabled": true
 }"""
    val js2 = s"""{
  "type": 14,
  "id": "Gobt7AiyQAfduRkW8Mk3naWbzH67Zsv9rdmgRNmon1Mb",
  "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
  "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
  "fee": $One,
  "timestamp": 1520945679531,
  "proofs": [
   "3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7"
  ],
  "version": 1,
  "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
  "isEnabled": false
 }"""
    val js3 = s"""{
  "type": 14,
  "id": "Gobt7AiyQAfduRkW8Mk3naWbzH67Zsv9rdmgRNmon1Mb",
  "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
  "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
  "fee": $One,
  "timestamp": 1520945679531,
  "proofs": [
   "3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7"
  ],
  "version": 1,
  "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
  "isEnabled": false
 }"""
    val js4 = s"""{
  "type": 14,
  "id": "Gobt7AiyQAfduRkW8Mk3naWbzH67Zsv9rdmgRNmon1Mb",
  "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
  "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
  "fee": $One,
  "timestamp": 1520945679531,
  "proofs": [
   "3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7"
  ],
  "version": 1,
  "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
  "isEnabled": false
 }"""

    val req = Json.parse(js1).validate[SignedSponsorFeeRequest].get.toTx.right.get
    req.proofs shouldBe Proofs(
      Seq(ByteStr.decodeBase58("3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7").get))
    req.fee shouldBe 100000000L
    req.isEnabled shouldBe true

    for (js <- Seq(js2, js3, js4)) {
      val req = Json.parse(js).validate[SignedSponsorFeeRequest].get.toTx.right.get
      Proofs(Seq(ByteStr.decodeBase58("3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7").get))
      req.fee shouldBe 100000000L
      req.isEnabled shouldBe false
    }
  }

  test("SetAssetScriptRequest json parsing works") {
    val json =
      """
        |{
        |"version": 1,
        |"senderPublicKey":"D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5",
        |"assetId":"Ha35nwsnmYxHRF8UmKG3S523BycBLZFU4FZnjXryKd4L",
        |"script":"base64:AQkAAGcAAAACAHho/EXujJiPAJUhuPXZYac+rt2jYg==",
        |"timestamp": 1520945679531,
        |"fee": 100000,
        |"proofs": [
        | "3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7"
        |]
        |}
      """.stripMargin
    val req = Json.parse(json).validate[SignedSetAssetScriptRequest].get
    req.assetId shouldBe "Ha35nwsnmYxHRF8UmKG3S523BycBLZFU4FZnjXryKd4L"
    req.proofs shouldBe Seq("3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7")
    req.fee shouldBe 100000L
    req.timestamp shouldBe 1520945679531L

    val tx = req.toTx.explicitGet()
    tx.assetId.base58 shouldBe "Ha35nwsnmYxHRF8UmKG3S523BycBLZFU4FZnjXryKd4L"
    tx.script shouldBe Some(Script.fromBase64String("base64:AQkAAGcAAAACAHho/EXujJiPAJUhuPXZYac+rt2jYg==").explicitGet())
    tx.fee shouldBe 100000L
    tx.timestamp shouldBe 1520945679531L
  }

}
