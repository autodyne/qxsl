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

import qxsl.extra.field.qxsl.*;
import qxsl.model.Item;
import qxsl.table.Tables;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link ZAllFormat}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/26
 *
 */
public final class ZAllFormatTest extends junit.framework.TestCase {
	private final ZAllFormat format = new ZAllFormat();
	private final Tables tables = new Tables();
	private final ArrayList<Band> bands = new ArrayList<>();
	private final Random random = new Random();
	public ZAllFormatTest() {
		bands.add(new Band(    3_500));
		bands.add(new Band(    7_000));
		bands.add(new Band(   14_000));
		bands.add(new Band(  144_000));
		bands.add(new Band(1_200_000));
		bands.add(new Band(5_600_000));
	}
	@Test
	public void testDecode() throws java.io.IOException {
		for(int numItems = 0; numItems <= 100; numItems++) {
			final ArrayList<Item> items = new ArrayList<>();
			for(int row = 0; row < numItems; row++) {
				final Item item = new Item();
				item.set(new Time());
				item.set(bands.get(random.nextInt(bands.size())));
				item.set(new Call(util.RandText.alnum(12)));
				item.set(new Mode(util.RandText.alnum(4)));
				item.set(new Note(util.RandText.alnum(50)));
				item.getRcvd().set(new RSTQ(random.nextInt(600)));
				item.getRcvd().set(new Code(util.RandText.alnum(7)));
				item.getSent().set(new RSTQ(random.nextInt(600)));
				item.getSent().set(new Code(util.RandText.alnum(7)));
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
