/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.sheet;

import java.util.Iterator;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * {@link SheetFactory}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/06/16
 */
public final class SheetFactoryTest extends Assertions {
	/**
	 * クラスパスにある全ての書式を返します。
	 *
	 *
	 * @return 書式のイテレータ
	 */
	public static Iterator<SheetFactory> testMethodSource() {
		return new SheetManager().iterator();
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testType(SheetFactory format) {
		assertThat(format.type()).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testName(SheetFactory format) {
		assertThat(format.name()).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testHelp(SheetFactory format) {
		assertThat(format.help()).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testExtensions(SheetFactory format) {
		assertThat(format.extensions()).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testToString(SheetFactory format) {
		assertThat(format.toString()).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testGetTableKey(SheetFactory format) {
		assertThat(format.getTableKey()).isNotEmpty();
	}
}
