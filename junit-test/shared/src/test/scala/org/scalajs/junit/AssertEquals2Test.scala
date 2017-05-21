package org.scalajs.junit

import org.junit.Assert._
import org.junit.Test
import org.scalajs.junit.utils._

class AssertEquals2Test {
  @Test def test(): Unit = {
    assertEquals("This is the message", false, true)
  }
}

class AssertEquals2TestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder =
    builder.assertion("test", "This is the message expected:<false> but was:<true>")
}
