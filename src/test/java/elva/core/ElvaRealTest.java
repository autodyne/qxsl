/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import java.math.BigDecimal;
import javax.script.ScriptException;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link ElvaReal}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/06
 */
public final class ElvaRealTest extends test.RandTest {
	private final ElvaReal R114 = new ElvaReal("114");
	private final ElvaReal R514 = new ElvaReal("514");
	private final ElvaReal R364 = new ElvaReal(364.0);
	private final ElvaReal R810 = new ElvaReal(810.0);
	private final BigDecimal D114 = new BigDecimal("114");
	private final BigDecimal D514 = new BigDecimal("514");

	@Test
	public void testValue() {
		assertThat(R114.value()).isEqualTo(D114);
		assertThat(R514.value()).isEqualTo(D514);
	}

	@Test
	public void testToBigDecimal() {
		assertThat(R114.toBigDecimal()).isEqualTo(D114);
		assertThat(R514.toBigDecimal()).isEqualTo(D514);
	}

	@Test
	public void testToString() {
		assertThat(R114).hasToString("114");
		assertThat(R514).hasToString("514");
		assertThat(R364).hasToString("364.0");
		assertThat(R810).hasToString("810.0");
	}

	@Test
	public void testAdd() {
		assertThat(R114.add(R514)).hasToString("628");
		assertThat(R364.add(R364)).hasToString("728.0");
	}

	@Test
	public void testSub() {
		assertThat(R114.sub(R514)).hasToString("-400");
		assertThat(R114.sub(R364)).hasToString("-250.0");
	}

	@Test
	public void testMul() {
		assertThat(R810.mul(R514)).hasToString("416340.0");
		assertThat(R364.mul(R364)).hasToString("132496.00");
	}

	@Test
	public void testDiv() {
		assertThat(R810.div(R514)).hasToString("1.575875486381323");
		assertThat(R810.div(R114)).hasToString("7.105263157894737");
	}

	@Test
	public void testMod() {
		assertThat(R810.mod(R514)).hasToString("296.0");
		assertThat(R514.mod(R364)).hasToString("150.0");
	}

	@Test
	public void testEquals() {
		assertThat(R114).isEqualTo(new ElvaReal(114.0));
		assertThat(R514).isEqualTo(new ElvaReal(514.0));
		assertThat(R364).isEqualTo(new ElvaReal("364"));
		assertThat(R810).isEqualTo(new ElvaReal("810"));
	}

	@Test
	public void testCompareTo() {
		assertThat(R114.compareTo(R114)).isZero();
		assertThat(R514.compareTo(R514)).isZero();
		assertThat(R810.compareTo(R364)).isPositive();
		assertThat(R364.compareTo(R810)).isNegative();
	}

	@Test
	public void testEval() throws ScriptException {
		final var elva = new elva.lang.ElvaRuntime();
		assertThat(elva.eval("114")).isEqualTo(114);
		assertThat(elva.eval("514")).isEqualTo(514);
	}
}
