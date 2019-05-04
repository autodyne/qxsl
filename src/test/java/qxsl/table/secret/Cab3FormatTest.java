/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.table.secret;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Random;
import org.junit.Test;
import qxsl.field.*;
import qxsl.model.Item;
import qxsl.table.Tables;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;

/**
 * {@see Cab3Format}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/05/04
 *
 */
public final class Cab3FormatTest extends junit.framework.TestCase {
	private final Cab3Format format = new Cab3Format();
	private final Tables tables = new Tables();
	private final ArrayList<Band> bands = new ArrayList<>();
	private final Random random = new Random();
	public Cab3FormatTest() {
		bands.add(new Band( 1_900));
		bands.add(new Band( 3_500));
		bands.add(new Band( 7_000));
		bands.add(new Band(10_000));
		bands.add(new Band(14_000));
		bands.add(new Band(18_000));
		bands.add(new Band(21_000));
		bands.add(new Band(28_000));
		bands.add(new Band(50_000));
	}
	@Test
	public void testDecode() throws java.io.IOException {
		for(int numItems = 0; numItems <= 100; numItems++) {
			final ArrayList<Item> items = new ArrayList<>();
			for(int row = 0; row < numItems; row++) {
				final Item item = new Item();
				item.set(new Time());
				item.set(bands.get(random.nextInt(bands.size())));
				item.set(new Call(util.RandText.alnum(13)));
				item.set(new Mode(util.RandText.alnum(2)));
				item.getRcvd().set(new RSTQ(random.nextInt(600)));
				item.getRcvd().set(new Code(util.RandText.alnum(6)));
				item.getSent().set(new RSTQ(random.nextInt(600)));
				item.getSent().set(new Code(util.RandText.alnum(6)));
				items.add(item);
			}
			final ByteArrayOutputStream os = new ByteArrayOutputStream();
			format.encode(os, items);
			final byte[] b = os.toByteArray();
			assertThat(format.decode(new ByteArrayInputStream(b)), is(items));
			assertThat(tables.decode(new ByteArrayInputStream(b)), is(items));
		}
	}
}
