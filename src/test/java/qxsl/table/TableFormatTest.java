/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.table;

import java.util.Iterator;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * {@link TableFactory}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/06/16
 */
public final class TableFormatTest extends Assertions {
	/**
	 * クラスパスにある全ての書式を返します。
	 *
	 *
	 * @return 書式のイテレータ
	 */
	public static Iterator<TableFactory> testMethodSource() {
		return new TableManager().iterator();
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testGetName(TableFactory format) {
		assertThat(format.getName()).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testToString(TableFactory format) {
		assertThat(format.toString()).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testGetDescription(TableFactory format) {
		assertThat(format.getDescription()).isNotNull();
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testGetExtensions(TableFactory format) {
		assertThat(format.getExtensions()).isNotEmpty();
	}
}
