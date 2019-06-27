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
 * {@link ZBinFormat}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/26
 *
 */
public final class ZBinFormatTest extends junit.framework.TestCase {
	private final ZBinFormat format = new ZBinFormat();
	private final Tables tables = new Tables();
	private final ArrayList<Band> bands = new ArrayList<>();
	private final ArrayList<Mode> modes = new ArrayList<>();
	private final ArrayList<Watt> watts = new ArrayList<>();
	private final Random random = new Random();
	public ZBinFormatTest() {
		bands.add(new Band(    3_500));
		bands.add(new Band(    7_000));
		bands.add(new Band(   14_000));
		bands.add(new Band(  144_000));
		bands.add(new Band(1_200_000));
		bands.add(new Band(5_600_000));
		modes.add(new Mode(  "CW"));
		modes.add(new Mode(  "AM"));
		modes.add(new Mode(  "FM"));
		modes.add(new Mode( "SSB"));
		modes.add(new Mode("RTTY"));
		watts.add(new Watt("H"));
		watts.add(new Watt("M"));
		watts.add(new Watt("L"));
		watts.add(new Watt("P"));
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
				item.set(modes.get(random.nextInt(modes.size())));
				item.set(new Note(util.RandText.alnum(66)));
				item.set(new Name(util.RandText.alnum(14)));
				item.getRcvd().set(new RSTQ(random.nextInt(600)));
				item.getRcvd().set(new Code(util.RandText.alnum(30)));
				item.getSent().set(new RSTQ(random.nextInt(600)));
				item.getSent().set(new Code(util.RandText.alnum(30)));
				item.getSent().set(watts.get(random.nextInt(watts.size())));
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
