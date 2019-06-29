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
 * {@link CBinFormat}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/05/03
 *
 */
public final class CBinFormatTest extends junit.framework.TestCase {
	private final CBinFormat format = new CBinFormat();
	private final TableFormats tables = new TableFormats();
	private final ArrayList<Freq> freqs = new ArrayList<>();
	private final ArrayList<Mode> modes = new ArrayList<>();
	private final Random random = new Random();
	public CBinFormatTest() {
		freqs.add(new Freq(    3_500));
		freqs.add(new Freq(    7_000));
		freqs.add(new Freq(   14_000));
		freqs.add(new Freq(  144_000));
		freqs.add(new Freq(1_200_000));
		freqs.add(new Freq(5_600_000));
		modes.add(new Mode(  "CW"));
		modes.add(new Mode(  "AM"));
		modes.add(new Mode(  "FM"));
		modes.add(new Mode( "SSB"));
		modes.add(new Mode("RTTY"));
	}
	@Test
	public void testDecode() throws java.io.IOException {
		for(int numItems = 0; numItems <= 100; numItems++) {
			final ArrayList<Item> items = new ArrayList<>();
			for(int row = 0; row < numItems; row++) {
				final Item item = new Item();
				item.add(new Time());
				item.add(freqs.get(random.nextInt(freqs.size())));
				item.add(new Call(util.RandText.alnum(19)));
				item.add(modes.get(random.nextInt(modes.size())));
				item.add(new Note(util.RandText.alnum(49)));
				item.add(new Name(util.RandText.alnum(19)));
				item.getRcvd().add(new Code(util.RandText.alnum(29)));
				item.getSent().add(new Code(util.RandText.alnum(29)));
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
