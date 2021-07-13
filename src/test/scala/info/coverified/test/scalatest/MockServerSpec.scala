/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.test.scalatest

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.core.WireMockConfiguration
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import sttp.client3.UriContext

trait MockServerSpec
    extends ZioSpec
    with BeforeAndAfterAll
    with BeforeAndAfterEach {
  protected val apiUrl = "127.0.0.1"
  protected val mockServer = new WireMockServer(
    WireMockConfiguration.wireMockConfig().bindAddress(apiUrl).dynamicPort()
  )

  protected def apiUri = uri"http://$apiUrl:${mockServer.port}/api/graphql"

  override protected def beforeAll(): Unit = {
    mockServer.start()
    super.beforeAll()
  }

  override protected def afterEach(): Unit = {
    mockServer.resetAll()
    super.afterEach()
  }

  override protected def afterAll(): Unit = {
    mockServer.stop()
    super.afterAll()
  }
}