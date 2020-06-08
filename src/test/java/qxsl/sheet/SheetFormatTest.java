/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.sheet;

import java.util.Iterator;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import static test.RandTest.*;

/**
 * {@link SheetFormat}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/06/16
 *
 */
public final class SheetFormatTest extends org.assertj.core.api.Assertions {
	/**
	 * クラスパスにある全ての書式を返します。
	 *
	 *
	 * @return 書式のイテレータ
	 */
	public static Iterator<SheetFormat> testMethodSource() {
		return new SheetFormats().iterator();
	}
	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testGetName(SheetFormat format) {
		assertThat(format.getName()).isNotEmpty();
	}
	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testToString(SheetFormat format) {
		assertThat(format.toString()).isNotEmpty();
	}
	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testGetDescription(SheetFormat format) {
		assertThat(format.getDescription()).isNotEmpty();
	}
	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testGetExtensions(SheetFormat format) {
		assertThat(format.getExtensions()).isNotEmpty();
	}
	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testGetTableKey(SheetFormat format) {
		assertThat(format.getTableKey()).isNotEmpty();
	}
}
