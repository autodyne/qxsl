/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.table;

import java.util.Iterator;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * {@link TableFactory}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/06/16
 */
public final class TableFactoryTest extends Assertions {
	@ParameterizedTest
	@MethodSource("source")
	public void testExtensions(TableFactory format) {
		assertThat(format.extensions()).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testHelp(TableFactory format) {
		assertThat(format.help()).isNotNull();
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testName(TableFactory format) {
		assertThat(format.name()).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testToString(TableFactory format) {
		assertThat(format.toString()).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testType(TableFactory format) {
		assertThat(format.type()).isNotEmpty();
	}

	public static final Iterator<TableFactory> source() {
		return new TableManager().iterator();
	}
}
