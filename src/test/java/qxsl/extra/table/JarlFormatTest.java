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
import java.io.IOException;
import java.util.ArrayList;
import java.util.Random;
import java.util.stream.IntStream;

import qxsl.extra.field.*;
import qxsl.model.Item;
import qxsl.table.TableFormats;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link JarlFormat}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/26
 *
 */
public final class JarlFormatTest extends junit.framework.TestCase {
	private final JarlFormat format = new JarlFormat();
	private final TableFormats tables = new TableFormats();
	private final ArrayList<Freq> freqs = new ArrayList<>();
	private final Random random = new Random();
	public JarlFormatTest() {
		freqs.add(new Freq(    3_500));
		freqs.add(new Freq(    7_000));
		freqs.add(new Freq(   14_000));
		freqs.add(new Freq(  144_000));
		freqs.add(new Freq(1_200_000));
		freqs.add(new Freq(5_600_000));
	}
	public static IntStream testMethodSource() {
		return IntStream.range(0, 100);
	}
	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testDecode(int numItems) throws IOException {
		final ArrayList<Item> items = new ArrayList<>();
		for(int row = 0; row < numItems; row++) {
			final Item item = new Item();
			item.add(new Time());
			item.add(freqs.get(random.nextInt(freqs.size())));
			item.add(new Call(util.RandText.alnum(13)));
			item.add(new Mode(util.RandText.alnum(5)));
			item.getRcvd().add(new RSTQ(random.nextInt(600)));
			item.getRcvd().add(new Code(util.RandText.alnum(7)));
			item.getSent().add(new RSTQ(random.nextInt(600)));
			item.getSent().add(new Code(util.RandText.alnum(7)));
			items.add(item);
		}
		final ByteArrayOutputStream os = new ByteArrayOutputStream();
		format.encode(os, items);
		final byte[] b = os.toByteArray();
		assertThat(format.decode(new ByteArrayInputStream(b))).isEqualTo(items);
		assertThat(tables.decode(new ByteArrayInputStream(b))).isEqualTo(items);
	}
}
