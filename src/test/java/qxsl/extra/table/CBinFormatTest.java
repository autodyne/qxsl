/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.stream.IntStream;

import qxsl.extra.field.*;
import qxsl.model.Item;
import qxsl.table.TableFormats;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import static test.RandTest.*;

/**
 * {@link CBinFormat}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/03
 *
 */
public final class CBinFormatTest extends org.assertj.core.api.Assertions {
	private final CBinFormat format = new CBinFormat();
	private final TableFormats tables = new TableFormats();
	private final ArrayList<Band> bands = new ArrayList<>();
	private final ArrayList<Mode> modes = new ArrayList<>();
	public CBinFormatTest() {
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
		modes.add(new Mode("JT65"));
		modes.add(new Mode( "FT8"));
		modes.add(new Mode( "FT4"));
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
			item.add(bands.get(randInt(bands.size())));
			item.add(new Call(alnum(19)));
			item.add(modes.get(randInt(modes.size())));
			item.add(new Note(alnum(49)));
			item.add(new Name(alnum(19)));
			item.getRcvd().add(new Code(alnum(29)));
			item.getSent().add(new Code(alnum(29)));
			items.add(item);
		}
		final ByteArrayOutputStream os = new ByteArrayOutputStream();
		format.encoder(os).encode(items);
		assertThat(tables.decode(os.toByteArray())).isEqualTo(items);
	}
}
