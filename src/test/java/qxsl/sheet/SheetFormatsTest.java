/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.sheet;

import org.junit.jupiter.api.Test;
import qxsl.extra.sheet.*;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link SheetFormats}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/03/12
 *
 */
public final class SheetFormatsTest extends test.RandTest {
	private final SheetFormats sheets = new SheetFormats();
	@Test
	public void testIterator() {
		assertThat(sheets.iterator()).hasNext();
	}
	@Test
	public void testGetFormat() {
		assertThat(sheets.getFormat("cab3")).isInstanceOf(Cab3Format.class);
		assertThat(sheets.getFormat("jarl")).isInstanceOf(JarlFormat.class);
	}
}