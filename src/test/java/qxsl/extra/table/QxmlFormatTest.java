/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.table;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Random;
import org.junit.Test;

import qxsl.extra.field.*;
import qxsl.model.Item;
import qxsl.table.TableFormats;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link QxmlFormat}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/26
 *
 */
public final class QxmlFormatTest extends junit.framework.TestCase {
	private final TableFormats tables = new TableFormats();
	private final Random random = new Random();
	@Test
	public void testDecode() throws Exception {
		final QxmlFormat format = new QxmlFormat();
		for(int numItems = 0; numItems <= 100; numItems++) {
			final ArrayList<Item> items = new ArrayList<>();
			for(int row = 0; row < numItems; row++) {
				final Item item = new Item();
				item.add(new Time());
				item.add(new Freq(random.nextInt(10_000_000)));
				item.add(new Call(util.RandText.alnum(10)));
				item.add(new Name(util.RandText.alnum(10)));
				item.add(new Note(util.RandText.alnum(10)));
				item.add(new Mode(util.RandText.alnum(10)));
				item.getRcvd().add(new RSTQ(random.nextInt(600)));
				item.getRcvd().add(new Code(util.RandText.alnum(10)));
				item.getRcvd().add(new Watt(util.RandText.alnum(10)));
				item.getSent().add(new RSTQ(random.nextInt(600)));
				item.getSent().add(new Code(util.RandText.alnum(10)));
				item.getSent().add(new Watt(util.RandText.alnum(10)));
				items.add(item);
			}
			final ByteArrayOutputStream os = new ByteArrayOutputStream();
			format.encode(os, items);
			final byte[] b = os.toByteArray();
			assertThat(format.decode(new ByteArrayInputStream(b))).isEqualTo(items);
			assertThat(tables.decode(new ByteArrayInputStream(b))).isEqualTo(items);
		}
	}
}
