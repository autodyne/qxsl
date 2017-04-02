/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.ruler;

import java.io.*;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.Test;
import qxsl.model.Item;
import qxsl.table.Tables;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;

/**
 * {@see Contest}クラスをALLJA1コンテストの規約でテストするクラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/26
 *
 */
public final class ContestTest extends junit.framework.TestCase {
	private final Contest ruler = Contest.forName("allja1.lisp");
	private final Map<String, List<Item>> logs = new HashMap<>();
	private final Tables tables = new Tables();
	public ContestTest() throws Exception {
		final Class cl = Contest.class;
		logs.put("ctxt", tables.decode(cl.getResource("allja1.ctxt")));
		logs.put("hl76", tables.decode(cl.getResource("allja1.hl76")));
		logs.put("jarl", tables.decode(cl.getResource("allja1.jarl")));
		logs.put("rtcl", tables.decode(cl.getResource("allja1.rtcl")));
		logs.put("zall", tables.decode(cl.getResource("allja1.zall")));
		logs.put("zdos", tables.decode(cl.getResource("allja1.zdos")));
	}
	@Test
	public void test() throws Exception {
		InputStream is = Contest.class.getResourceAsStream("allja1.test");
		BufferedReader br = new BufferedReader(new InputStreamReader(is));
		String line = null;
		while((line = br.readLine()) != null) {
			for(String tfmt: logs.keySet()) {
				final String[] vals = line.split(",", 3);
				final Section sec = ruler.getSection(vals[0].trim());
				final Summary sum = new Summary(logs.get(tfmt), sec);
				assertThat(sum.calls(), is(Integer.parseInt(vals[1].trim())));
				assertThat(sum.mults(), is(Integer.parseInt(vals[2].trim())));
			}
		}
	}
}
