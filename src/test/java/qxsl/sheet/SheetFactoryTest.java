/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.sheet;

import java.util.Iterator;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * {@link SheetFactory}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/06/16
 */
public final class SheetFactoryTest extends Assertions {
	@ParameterizedTest
	@MethodSource("source")
	public void testExtensions(SheetFactory format) {
		assertThat(format.extensions()).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testGetTableKey(SheetFactory format) {
		assertThat(format.getTableKey()).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testHelp(SheetFactory format) {
		assertThat(format.help()).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testName(SheetFactory format) {
		assertThat(format.name()).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testToString(SheetFactory format) {
		assertThat(format.toString()).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testType(SheetFactory format) {
		assertThat(format.type()).isNotEmpty();
	}

	public static final Iterator<SheetFactory> source() {
		return new SheetManager().iterator();
	}
}
