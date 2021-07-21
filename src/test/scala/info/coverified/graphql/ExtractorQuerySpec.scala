/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.graphql

import com.github.tomakehurst.wiremock.client.WireMock.{
  aResponse,
  post,
  postRequestedFor,
  urlEqualTo
}
import com.github.tomakehurst.wiremock.matching.EqualToPattern
import info.coverified.test.scalatest.{GraphQlHelper, MockServerSpec}

class ExtractorQuerySpec extends MockServerSpec with GraphQlHelper {
  "Preparing the GrapGL queries" when {
    "asking for entries" should {
      "bring correct query" in {
        mockServer.stubFor(
          post(urlEqualTo("/api/graphql")).willReturn(
            aResponse()
              .withHeader("Content-Type", "text/plain")
              .withBody("""
            |{
            |  "data": {
            |    "allEntries": [
            |      {
            |        "id": "ckr7fgk9d0798fdo8sas1v1us",
            |        "name": "",
            |        "hasBeenTagged": false,
            |        "url": {
            |          "id": "ckr7fdbuw0218fdo803zdc1hy"
            |        },
            |        "tags": [],
            |        "language": null,
            |        "content": "",
            |        "summary": "",
            |        "date": null,
            |        "disabled": false
            |      }
            |    ]
            |  }
            |}
            |""".stripMargin)
          )
        )

        evaluateWithHttpClientLayer(
          Connector.sendRequest(
            ExtractorQuery
              .existingEntry("ckr7fdbuw0218fdo803zdc1hy")
              .toRequest(apiUri)
          )
        )

        noException shouldBe thrownBy {
          mockServer.verify(
            postRequestedFor(urlEqualTo("/api/graphql")).withRequestBody(
              new EqualToPattern(
                "{\"query\":\"query{allEntries(where:{url:{id:\\\"ckr7fdbuw0218fdo803zdc1hy\\\"}},orderBy:[],skip:0){id name content summary url{id} date disabled}}\",\"variables\":{}}"
              )
            )
          )
        }
      }
    }
  }
}
